{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module THIS.Config.Executor where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import Control.Failure
import Data.Maybe
import Data.Object
import Data.Object.Yaml
import Text.Printf

import THIS.Types
import THIS.Yaml

loadExecutor :: FilePath -> YamlM Executor
loadExecutor name = do
  object <- loadYaml "executors" name
  ErrorT (return $ convertExecutor object)

convertExecutor :: StringObject -> Either YamlError Executor
convertExecutor object = do
  aclist <- getOptional "actions" [] object
  acs <- concat <$> (mapM convertAction =<< getMapping object)
  return $ Executor {
             exActions = aclist,
             exConfigs = acs }

convertAction :: (String, StringObject) -> Either YamlError [(String, ActionConfig)]
convertAction ("actions", _) = return []
convertAction (name, object) = do
  cmds <- get "commands" object
  return [(name, ActionConfig {
                  acCommands = cmds } )]

lookupAction :: String -> Executor -> Maybe ActionConfig
lookupAction act (Executor _ pairs) =
  case lookup act pairs of
    Just ac -> Just ac
    Nothing -> lookup "$$" pairs

data SItem = ACTION | VAR String | LITERAL String
  deriving (Eq, Show)

parseString :: String -> Either String [SItem]
parseString str = reverse <$> go False "" [] str
  where
    go False _ acc []                = return acc
    go True  v acc []                = Left $ "No ending '}' ? : " ++ v
    go False _ acc ('$':'$':s)       = go False "" (ACTION:acc) s
    go False _ acc ('$':'{':s)       = go True  "" acc          s
    go True  v acc ('}':s)           = go False "" (VAR v:acc)  s
    go True  v acc (c:s)             = go True  (v ++ [c]) acc  s
    go False _ (LITERAL o:acc) (c:s) = go False "" (LITERAL (o ++ [c]):acc) s
    go False _ acc (c:s)             = go False "" (LITERAL [c]:acc) s

eval :: String -> [(String, String)] -> String -> Either String String
eval action pairs str = concatMap go <$> parseString str
  where
    go ACTION = action
    go (VAR v) = fromMaybe "" $ lookup v pairs
    go (LITERAL s) = s
