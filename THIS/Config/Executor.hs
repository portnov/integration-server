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

loadExecutor :: FilePath -> YamlM (FilePath, Executor)
loadExecutor name = do
  (path, object) <- loadYaml "executors" name
  r <- ErrorT $ return $ convertExecutor object
  return (path, r)

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
  cmds <- get "commands" object :: Either YamlError [String]
  return [(name, ActionConfig {
                  acCommands = cmds } )]

lookupAction :: String -> Executor -> Maybe ActionConfig
lookupAction act (Executor _ pairs) =
  case lookup act pairs of
    Just ac -> Just ac
    Nothing -> lookup "$$" pairs

