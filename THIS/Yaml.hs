{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
-- | YAML loading utilities
module THIS.Yaml where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Failure
import Control.Exception as E
import Data.Char
import Data.List
import Data.Object
import Data.Object.Yaml
import System.FilePath
import System.Directory
import System.Environment

import THIS.Types

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

class (Monad m, Failure ErrorMessage m) => Got k v m a where
  get :: k -> Object k v -> m a

  getOptional :: k -> a -> Object k v -> m a

instance Got String String (Either ErrorMessage) StringObject where
  get k (Mapping pairs) =
    case lookup k pairs :: Maybe StringObject of
      Nothing -> failure $ "Key not found: " ++ k
      Just v -> return v
  get _ obj = failure $ "Expected mapping, but got: " ++ show obj

  getOptional k def (Mapping pairs) =
    case lookup k pairs of
      Nothing -> return def
      Just v  -> return v
  getOptional _ _ obj = failure $ "Expected mapping, but got: " ++ show obj


instance Got String String (Either ErrorMessage) String where
  get k o = getString =<< get k o
  getOptional k d o = getString =<< getOptional k (Scalar d) o

instance Got String String (Either ErrorMessage) [String] where
  get k o = mapM getString =<< get k o
  getOptional k d o = mapM getString =<< getOptional k (map Scalar d) o

instance Got String String (Either ErrorMessage) Integer where
  get k o = getInteger =<< get k o
  getOptional k d o = getInteger =<< getOptional k (show d) o

instance Got String String (Either ErrorMessage) Bool where
  get k o = readBool =<< get k o
  getOptional k d o = readBool =<< getOptional k (show d) o

readBool :: String -> Either ErrorMessage Bool
readBool s = do
  let str = map toLower s
  if str `elem` ["true", "yes", "on", "1"]
    then return True
    else if str `elem` ["false", "no", "off", "0"]
           then return False
           else failure $ "Cannot parse as boolean value: " ++ s

instance Got String String (Either ErrorMessage) Int where
  get k o = readInt =<< get k o
  getOptional k d o = readInt =<< getOptional k (show d) o

readInt :: String -> Either ErrorMessage Int
readInt s =
  case reads s of
    [(n, "")] -> return n
    _ -> failure $ "Cannot parse int: " ++ s

instance Got String String (Either ErrorMessage) [StringObject] where
  get k o = getSequence =<< get k o
  getOptional k d o = getSequence =<< getOptional k (Sequence d) o

instance Got String String (Either ErrorMessage) [(String, StringObject)] where
  get k o = do
    x <- get k o
    case x of
      Mapping pairs -> return pairs
      _ -> failure $ "Expected mapping, but got: " ++ show x

  getOptional k d o = do
    x <- getOptional k (Mapping d) o
    case x of
      Mapping pairs -> return pairs
      _ -> failure $ "Expected mapping, but got: " ++ show x

instance Got String String (Either ErrorMessage) [(String, String)] where
  get k o = getPairs =<< get k o
  
  getOptional k d o = getPairs =<< getOptional k (Mapping [(k, Scalar v) | (k,v) <- d]) o

getSequence :: StringObject -> Either ErrorMessage [StringObject]
getSequence (Sequence list) = return list
getSequence (Scalar x) = return [Scalar x]
getSequence x = failure $ "Expected sequence, but got: " ++ show x

getMapping :: StringObject -> Either ErrorMessage [(String, StringObject)]
getMapping (Mapping pairs) = return pairs
getMapping x = failure $ "Expected mapping, but got: " ++ show x

getString :: StringObject -> Either ErrorMessage String
getString (Scalar x) = return x
getString y = failure $ "Expected scalar, but got: " ++ show y

getInteger :: String -> Either ErrorMessage Integer
getInteger s =
  let (ds,suf) = span isDigit s
  in  if null ds
        then failure $ "Not an integer: " ++ s
        else case lookup (map toLower suf) suffixes of
               Nothing -> failure $ "Unknown suffix: " ++ suf
               Just c  -> return $ read ds * c

getPairs :: StringObject -> Either ErrorMessage [(String, String)]
getPairs object = concatMap go <$> getMapping object
  where
    go (name, Scalar val) = [(name, val)]
    go (_,_) = []

getInherit :: StringObject -> Either ErrorMessage (Maybe String, StringObject)
getInherit x@(Mapping ps) = case lookupRemove "inherit" ps of
                              (Nothing, _) -> return (Nothing, x)
                              (Just (Scalar y), s) -> return (Just y, Mapping s)
                              (Just y, _) -> failure $ "Inherit: should be Scalar, but got " ++ show y
getInherit x            = return (Nothing, x)

merge :: StringObject -> StringObject -> StringObject
merge (Mapping ps1) (Mapping ps2) = Mapping $ mergeMap ps1 ps2
merge (Sequence _)  (Sequence ls) = Sequence ls
merge (Scalar _)    (Scalar v)    = Scalar v
merge _             y             = y

mergeMap :: [(String, StringObject)] -> [(String, StringObject)] -> [(String, StringObject)]
mergeMap ps [] = ps
mergeMap ps ((k,v):other) =
  case lookupRemove k ps of
    (Nothing,s) -> (k,v): mergeMap s other
    (Just u, s) -> (k, merge u v): mergeMap s other

lookupRemove :: (Eq k) => k -> [(k,v)] -> (Maybe v, [(k,v)])
lookupRemove k pairs = go [] k pairs
  where
    go acc _ [] = (Nothing, acc)
    go acc k ((k', v): other)
      | k == k' = (Just v, acc ++ other)
      | otherwise = go (acc ++ [(k',v)]) k other

loadYaml :: String -> String -> THIS (FilePath, StringObject)
loadYaml kind name = do
  let yamlName = if ".yaml" `isSuffixOf` name
                   then name
                   else name ++ ".yaml"
  home <- liftIO $ getEnv "HOME"
  let homePath = home </> ".config" </> "this" </> kind </> yamlName
      etcPath  = "/etc/this" </> kind </> yamlName
  he <- liftIO $ doesFileExist homePath
  path <- if he
            then return homePath
            else do
                 ee <- liftIO $ doesFileExist etcPath
                 if ee
                   then return etcPath
                   else failure $ "YAML file not found neither in ~ nor /etc: " ++ kind </> yamlName
  mby <- liftIO (decodeFile path :: IO (Either ParseException StringObject))
  case mby of
    Left err -> failure (show err)
    Right yaml -> case getInherit yaml of
                    Left err -> failure err
                    Right (Just p, yaml') -> do
                        (parentPath, parent) <- loadYaml kind p
                        let fullPath = parentPath ++ " + " ++ parentPath
                        return (fullPath, merge parent yaml')
                    Right (Nothing, _) -> return (path, yaml)

forceTHIS :: (Exception e) => (String -> e) -> THIS a -> IO a
forceTHIS efn m = do
  x <- runErrorT m
  case x of
    Left (Message err) -> throw (efn err) 
    Left (YamlError e) -> throw (efn $ show e)
    Right val -> return val
    
