{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Executors loader
module THIS.Config.Executor
  (loadExecutor,
   lookupAction
  ) where

import Control.Applicative
import Control.Monad.Error
import Data.Maybe
import Data.Object

import THIS.Types
import THIS.Util
import THIS.Yaml

-- | Load executor by name.
-- Returns path to loaded executor and executor itself.
loadExecutor :: String -> THIS (FilePath, Executor)
loadExecutor name = do
  (path, object) <- loadYaml "executors" name
  r <- liftEither $ convertExecutor object
  return (path, r)

convertExecutor :: StringObject -> Either ErrorMessage Executor
convertExecutor object = do
  aclist <- getOptional "actions" [] object
  acs <- concat <$> (mapM convertAction =<< getMapping object)
  return $ Executor {
             exActions = aclist,
             exConfigs = acs }

convertAction :: (String, StringObject) -> Either ErrorMessage [(String, ActionConfig)]
convertAction ("actions", _) = return []
convertAction (name, object) = do
  cmds <- get "commands" object :: Either ErrorMessage [String]
  return [(name, ActionConfig {
                  acCommands = cmds } )]

-- | Look up for an action in the executor.
lookupAction :: String -> Executor -> Maybe ActionConfig
lookupAction action (Executor _ pairs) =
  lookup action pairs `mplus` lookup "$$" pairs

