{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module THIS.Config.ProjectConfig where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import Control.Failure
import Data.Object
import Data.Object.Yaml
import System.Environment
import System.FilePath
import System.FilePath.Glob
import System.Directory

import THIS.Types
import THIS.Yaml

loadHost :: FilePath -> YamlM (String, HostConfig)
loadHost path = do
  x <- liftIO (decodeFile path :: IO (Either ParseException StringObject))
  case x of
    Left err -> failure (show err)
    Right object -> ErrorT (return $ convertHost ("", object))

loadCommonHosts :: YamlM [(String, HostConfig)]
loadCommonHosts = do
  home <- liftIO $ getEnv "HOME"
  let homeMask = home </> ".config" </> "this" </> "hosts" </> "*.yaml"
      etcMask  = "/etc/this" </> "hosts" </> "*.yaml"
  homePaths <- liftIO $ glob (compile homeMask)
  etcPaths  <- liftIO $ glob (compile etcMask)
  forM (homePaths ++ etcPaths) $ \path -> do
      let name = dropExtension (takeFileName path)
      host <- loadHost path
      return (name, snd host)

loadProjectConfig :: String -> [(String, HostConfig)] -> YamlM ProjectConfig
loadProjectConfig name hosts = do
  object <- loadYaml "projects" name
  ErrorT (return $ convertProject hosts object)

convertProject :: [(String, HostConfig)] -> StringObject -> Either YamlError ProjectConfig
convertProject commonHosts object = do
  dir <- get "directory" object
  let this = HostConfig {
               hcHostname = "localhost",
               hcPath     = dir,
               hcVM       = Nothing }
  hosts <- mapM convertHost =<< get "hosts" object
  let allHosts = [("this", this)] ++ commonHosts ++ hosts
  phases <- mapM (convertPhase allHosts) =<< get "phases" object
  env <- getPairs object
  return $ ProjectConfig {
             pcDirectory = dir,
             pcHosts = allHosts,
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
                      <$> getOptional "hypervisor" "qemu:///system" object
                      <*> get "empty" object
                      <*> get "template" object
                      <*> get "name" object
                      <*> getOptional "snapshot" "" object
                      <*> getPairs object)
            _ -> failure $ "Unknown host type: " ++ ht
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
  shutdown <- getOptional "shutdown" True object
  preexec  <- getOptional "pre-execute" [] object
  executor <- get "executor" object
  actions  <- getOptional "actions" [] object
  parser   <- getOptional "parser" executor object
  files    <- mapM convertFiles =<< getOptional "files" [] object
  shell    <- getOptional "shell" [] object
  env      <- getPairs object
  return (name, Phase {
                   phWhere      = whr,
                   phShutdownVM = shutdown,
                   phPreExecute = preexec,
                   phExecutor   = executor,
                   phActions    = actions,
                   phParser     = parser,
                   phFiles      = files,
                   phShellCommands = shell,
                   phEnvironment = env } )

convertFiles :: (String, StringObject) -> Either YamlError (String, [FilePath])
convertFiles (name, object) = do
  files <- mapM getString =<< getSequence object
  return (name, files)

