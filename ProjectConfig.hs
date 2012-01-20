{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module ProjectConfig where

import Control.Applicative
import Control.Monad
import Control.Monad.Instances
import Control.Failure
import Data.Char
import Data.Object
import Data.Object.Yaml

import Types

suffixes :: [(String, Integer)]
suffixes = concatMap (\(n, ss) -> [(s, n) | s <- ss]) $
  [(1024,                ["k", "kib"]),
   (1024*1024,           ["m", "mib"]),
   (1024*1024*1024,      ["g", "gib"]),
   (1024*1024*1024*1024, ["t", "tib"]),
   (1000,                ["kb"]),
   (1000000,             ["mb"]),
   (1000000000,          ["gb"]),
   (1000000000000,       ["tb"]) ]

instance Failure e (Either e) where
  failure e = Left e

class Got a where
  get :: String -> StringObject -> Either String a

instance Got StringObject where
  get k (Mapping pairs) =
    case lookup k pairs of
      Nothing -> fail $ "Key not found: " ++ k
      Just v -> return v
  get _ obj = fail $ "Expected mapping, but got: " ++ show obj

getS :: String -> StringObject -> Either String String
getS k o = getString =<< get k o

instance Got String where
  get = getS

instance Got Integer where
  get k o = getInteger =<< get k o

instance Got Bool where
  get k o = do
    s <- get k o
    let str = map toLower s
    if str `elem` ["true", "yes", "on", "1"]
      then return True
      else if str `elem` ["false", "no", "off", "0"]
             then return False
             else fail $ "Cannot parse as boolean value: " ++ s

instance Got [StringObject] where
  get k o = getSequence =<< get k o

instance Got [(String, StringObject)] where
  get k o = do
    x <- get k o
    case x of
      Mapping pairs -> return pairs
      _ -> fail $ "Expected mapping, but got: " ++ show x

loadProjectConfig :: FilePath -> IO (Either String ProjectConfig)
loadProjectConfig path = do
  x <- decodeFile path
  case x of
    Left err -> fail $ show (err :: ParseException)
    Right object -> return $ convertProject object

convertProject :: StringObject -> Either String ProjectConfig
convertProject object = do
  dir <- get "directory" object
  hosts <- mapM convertHost =<< get "hosts" object
  phases <- mapM (convertPhase hosts) =<< get "phases" object
  return $ ProjectConfig {
             pcDirectory = dir,
             pcHosts = hosts,
             pcPhases = phases }

getInteger :: String -> Either String Integer
getInteger s =
  let (ds,suf) = span isDigit s
  in  if null ds
        then fail $ "Not an integer: " ++ s
        else case lookup (map toLower suf) suffixes of
               Nothing -> fail $ "Unknown suffix: " ++ suf
               Just c  -> return $ read ds * c

convertHost :: (String, StringObject) -> Either String (String, HostConfig)
convertHost (name, object) = do
  ht <- getString =<< get' "type" (Scalar "host") object
  hostname <- get "host" object
  path <- get "path" object
  mbvm <- case ht of
            "host" -> return Nothing
            "vm" -> Just <$> (VMConfig
                      <$> get "empty" object
                      <*> get "template" object
                      <*> get "name" object
                      <*> get "memory" object
                      <*> get "storage-size" object
                      <*> get "cdrom-image" object)
            _ -> fail $ "Unknown host type: " ++ ht
  return (name, HostConfig {
             hcHostname = hostname,
             hcPath = path,
             hcVM = mbvm } )

convertPhase :: [(String, HostConfig)] -> (String, StringObject) -> Either String (String, Phase)
convertPhase hosts (name, object) = do
  whs <- get "where" object
  whr <- case lookup whs hosts of
           Nothing -> fail $ "Unknown host: " ++ whs
           Just hc -> return hc
  preexec <- mapM getString =<< getSequence =<< get' "pre-execute" (Sequence []) object
  executor <- get "executor" object
  parser <- getString =<< get' "parser" (Scalar executor) object
  files <- mapM convertFiles =<< getMapping =<< get' "files" (Mapping []) object
  shell <- mapM getString =<< getSequence =<< get' "shell" (Sequence []) object
  return (name, Phase {
                   phWhere = whr,
                   phPreExecute = preexec,
                   phExecutor = executor,
                   phParser = parser,
                   phFiles = files,
                   phShellCommands = shell } )

convertFiles :: (String, StringObject) -> Either String (String, [FilePath])
convertFiles (name, object) = do
  files <- mapM getString =<< getSequence object
  return (name, files)

toMaybe :: Either String a -> Maybe a
toMaybe (Right x) = Just x
toMaybe _         = Nothing

getSequence :: StringObject -> Either String [StringObject]
getSequence (Sequence list) = return list
getSequence (Scalar x) = return [Scalar x]
getSequence x = fail $ "Expected sequence, but got: " ++ show x

getMapping :: StringObject -> Either String [(String, StringObject)]
getMapping (Mapping pairs) = return pairs
getMapping x = fail $ "Expected mapping, but got: " ++ show x

getString :: StringObject -> Either String String
getString (Scalar x) = return x
getString y = fail $ "Expected scalar, but got: " ++ show y

get' :: String -> StringObject -> StringObject -> Either String StringObject 
get' k def (Mapping pairs) =
  case lookup k pairs of
    Nothing -> return def
    Just v  -> return v
get' _ _ obj = fail $ "Expected mapping, but got: " ++ show obj

