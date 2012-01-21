{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module ProjectConfig where

import Control.Applicative
import Data.Object
import Data.Object.Yaml

import Types
import Yaml

loadProjectConfig :: FilePath -> IO (Either YamlError ProjectConfig)
loadProjectConfig path = do
  x <- decodeFile path
  case x of
    Left err -> fail $ show (err :: ParseException)
    Right object -> return $ convertProject object

convertProject :: StringObject -> Either YamlError ProjectConfig
convertProject object = do
  dir <- get "directory" object
  hosts <- mapM convertHost =<< get "hosts" object
  phases <- mapM (convertPhase hosts) =<< get "phases" object
  return $ ProjectConfig {
             pcDirectory = dir,
             pcHosts = hosts,
             pcPhases = phases }

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
                      <*> get "memory" object
                      <*> get "storage-size" object
                      <*> get "cdrom-image" object)
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
  parser <- getOptional "parser" executor object
  files <- mapM convertFiles =<< getOptional "files" [] object
  shell <- getOptional "shell" [] object
  return (name, Phase {
                   phWhere = whr,
                   phPreExecute = preexec,
                   phExecutor = executor,
                   phParser = parser,
                   phFiles = files,
                   phShellCommands = shell } )

convertFiles :: (String, StringObject) -> Either YamlError (String, [FilePath])
convertFiles (name, object) = do
  files <- mapM getString =<< getSequence object
  return (name, files)

