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
import THIS.Templates.Text

loadExecutor :: FilePath -> YamlM Executor
loadExecutor name = do
  (path, object) <- loadYaml "executors" name
  ErrorT $ return $ convertExecutor path object

convertExecutor :: FilePath -> StringObject -> Either YamlError Executor
convertExecutor path object = do
  aclist <- getOptional "actions" [] object
  acs <- concat <$> (mapM (convertAction path object) =<< getMapping object)
  return $ Executor {
             exActions = aclist,
             exConfigs = acs }

convertAction :: FilePath -> StringObject -> (String, StringObject) -> Either YamlError [(String, ActionConfig)]
convertAction _ _ ("actions", _) = return []
convertAction path executor (name, object) = do
  cmds <- get "commands" object :: Either YamlError [String]
  vars <- getPairs executor
  cmds' <- mapM (evalTemplate path executor vars) cmds
  return [(name, ActionConfig {
                  acCommands = cmds' } )]

lookupAction :: String -> Executor -> Maybe ActionConfig
lookupAction act (Executor _ pairs) =
  case lookup act pairs of
    Just ac -> Just ac
    Nothing -> lookup "$$" pairs

