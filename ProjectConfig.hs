{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module ProjectConfig where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import Control.Failure
import Data.Object
import Data.Object.Yaml

import Types
import Yaml

loadProjectConfig :: String -> YamlM ProjectConfig
loadProjectConfig name = do
  object <- loadYaml "projects" name
  ErrorT (return $ convertProject object)

convertProject :: StringObject -> Either YamlError ProjectConfig
convertProject object = do
  dir <- get "directory" object
  hosts <- mapM convertHost =<< get "hosts" object
  phases <- mapM (convertPhase hosts) =<< get "phases" object
  env <- getPairs object
  return $ ProjectConfig {
             pcDirectory = dir,
             pcHosts = hosts,
             pcPhases = phases,
             pcEnvironment = env }

convertVar :: (String, StringObject) -> Either YamlError (String, String)
convertVar (name, object) = do
  val <- getString object
  return (name, val)

convertHost :: (String, StringObject) -> Either YamlError (String, HostConfig)
convertHost (name, object) = do
  ht <- getOptional "type" "host" object
  hostname <- get "host" object
  path <- get "path" object
  mbvm <- case ht of
            "host" -> return Nothing
            "vm" -> Just <$> (VMConfig
                      <$> get "empty" object
                      <*> get "template" object
                      <*> get "name" object
                      <*> getPairs object)
            _ -> fail $ "Unknown host type: " ++ ht
  return (name, HostConfig {
             hcHostname = hostname,
             hcPath = path,
             hcVM = mbvm } )

convertPhase :: [(String, HostConfig)] -> (String, StringObject) -> Either YamlError (String, Phase)
convertPhase hosts (name, object) = do
  whs <- get "where" object
  whr <- case lookup whs hosts of
           Nothing -> fail $ "Unknown host: " ++ whs
           Just hc -> return hc
  preexec <- getOptional "pre-execute" [] object
  executor <- get "executor" object
  actions <- getOptional "actions" [] object
  parser <- getOptional "parser" executor object
  files <- mapM convertFiles =<< getOptional "files" [] object
  shell <- getOptional "shell" [] object
  env <- getPairs object
  return (name, Phase {
                   phWhere = whr,
                   phPreExecute = preexec,
                   phExecutor = executor,
                   phActions = actions,
                   phParser = parser,
                   phFiles = files,
                   phShellCommands = shell,
                   phEnvironment = env } )

convertFiles :: (String, StringObject) -> Either YamlError (String, [FilePath])
convertFiles (name, object) = do
  files <- mapM getString =<< getSequence object
  return (name, files)

