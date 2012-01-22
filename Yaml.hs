{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Yaml where

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

class (Monad m, Failure YamlError m) => Got k v m a where
  get :: k -> Object k v -> m a

  getOptional :: k -> a -> Object k v -> m a

instance Got String String (Either YamlError) StringObject where
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


instance Got String String (Either YamlError) String where
  get k o = getString =<< get k o
  getOptional k d o = getString =<< getOptional k (Scalar d) o

instance Got String String (Either YamlError) [String] where
  get k o = mapM getString =<< get k o
  getOptional k d o = mapM getString =<< getOptional k (map Scalar d) o

instance Got String String (Either YamlError) Integer where
  get k o = getInteger =<< get k o
  getOptional k d o = getInteger =<< getOptional k (show d) o

instance Got String String (Either YamlError) Bool where
  get k o = readBool =<< get k o
  getOptional k d o = readBool =<< getOptional k (show d) o

readBool :: String -> Either YamlError Bool
readBool s = do
  let str = map toLower s
  if str `elem` ["true", "yes", "on", "1"]
    then return True
    else if str `elem` ["false", "no", "off", "0"]
           then return False
           else failure $ "Cannot parse as boolean value: " ++ s

instance Got String String (Either YamlError) [StringObject] where
  get k o = getSequence =<< get k o
  getOptional k d o = getSequence =<< getOptional k (Sequence d) o

instance Got String String (Either YamlError) [(String, StringObject)] where
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

getSequence :: StringObject -> Either YamlError [StringObject]
getSequence (Sequence list) = return list
getSequence (Scalar x) = return [Scalar x]
getSequence x = failure $ "Expected sequence, but got: " ++ show x

getMapping :: StringObject -> Either YamlError [(String, StringObject)]
getMapping (Mapping pairs) = return pairs
getMapping x = failure $ "Expected mapping, but got: " ++ show x

getString :: StringObject -> Either YamlError String
getString (Scalar x) = return x
getString y = failure $ "Expected scalar, but got: " ++ show y

getInteger :: String -> Either YamlError Integer
getInteger s =
  let (ds,suf) = span isDigit s
  in  if null ds
        then failure $ "Not an integer: " ++ s
        else case lookup (map toLower suf) suffixes of
               Nothing -> failure $ "Unknown suffix: " ++ suf
               Just c  -> return $ read ds * c

