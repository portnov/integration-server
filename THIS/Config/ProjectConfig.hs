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
import THIS.Templates.Text

import Debug.Trace

loadHost :: FilePath -> THIS (String, HostConfig)
loadHost path = do
  x <- liftIO (decodeFile path :: IO (Either ParseException StringObject))
  case x of
    Left err -> failure (show err)
    Right object -> liftEither $ convertHost ("", object)

loadCommonHosts :: THIS [(String, HostConfig)]
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

loadProjectConfig :: String -> [(String, String)] -> [(String, HostConfig)] -> THIS (FilePath, StringObject, ProjectConfig)
loadProjectConfig name vars hosts = do
  (path, object) <- loadYaml "projects" name
  pc <- ErrorT $ return $ convertProject path vars hosts object
  return (path, object, pc)

convertProject :: FilePath -> [(String, String)] -> [(String, HostConfig)] -> StringObject -> Either ErrorMessage ProjectConfig
convertProject path vars commonHosts object = do
  dir <- get "directory" object
  let this = HostConfig {
               hcHostname = "localhost",
               hcPath     = dir,
               hcVM       = Nothing,
               hcParams   = [("host", "localhost")] }
  title <- getOptional "title" (takeFileName path) object
  owner <- getOptional "owner" "admin" object
  hosts <- mapM convertHost =<< get "hosts" object
  let allHosts = [("this", this)] ++ commonHosts ++ hosts
  phases <- mapM (convertPhase path object vars allHosts) =<< get "phases" object
  env <- getPairs object
  return $ ProjectConfig {
             pcDirectory = dir,
             pcTitle = title,
             pcOwner = owner,
             pcHosts = allHosts,
             pcPhases = phases,
             pcEnvironment = env }

convertVar :: (String, StringObject) -> Either ErrorMessage (String, String)
convertVar (name, object) = do
  val <- getString object
  return (name, val)

convertHost :: (String, StringObject) -> Either ErrorMessage (String, HostConfig)
convertHost (name, object) = do
  ht <- getOptional "type" "host" object
  hostname <- get "host" object
  path <- get "path" object
  mbvm <- case ht of
            "host" -> return Nothing
            "vm" -> Just <$> (VMConfig
                      <$> getOptional "hypervisor" "qemu:///system" object
                      <*> getOptional "empty" False object
                      <*> get "template" object
                      <*> getOptional "name" hostname object
                      <*> getOptional "snapshot" "" object)
            _ -> failure $ "Unknown host type: " ++ ht
  params <- getPairs object
  return (name, HostConfig {
             hcHostname = hostname,
             hcPath = path,
             hcVM = mbvm,
             hcParams = params } )

convertPhase :: FilePath -> StringObject -> [(String, String)] -> [(String, HostConfig)] -> (String, StringObject) -> Either ErrorMessage (String, Phase)
convertPhase path project vars hosts (name, object) = do
  pairs <- getPairs object
  let eval x = liftError ParsecError $ evalTemplate path project (pairs ++ vars) x
  whs <- get "where" object
  whs' <- eval whs
  whr <- case lookup whs' hosts of
           Nothing -> fail $ "Unknown host: " ++ whs'
           Just hc -> return hc
  shutdown <- getOptional "shutdown" True object
  preexec  <- mapM eval =<< getOptional "pre-execute" [] object
  executor <- get "executor" object
  actions  <- getOptional "actions" [] object
  parser   <- getOptional "parser" executor object
  newFilesT <- getPairs =<< getOptional "create-files" (Mapping []) object
  newTs    <- mapM eval (map fst newFilesT)
  newFs    <- mapM eval (map snd newFilesT)
  let newFiles = zip newTs newFs
  files    <- mapM convertFiles =<< getOptional "files" [] object
  shell    <- getOptional "shell" [] object
  shell'   <- mapM eval shell
  env      <- getPairs object
  return (name, Phase {
                   phWhere      = whr,
                   phShutdownVM = shutdown,
                   phPreExecute = preexec,
                   phExecutor   = executor,
                   phActions    = actions,
                   phParser     = parser,
                   phCreateFiles = newFiles,
                   phFiles      = files,
                   phShellCommands = shell',
                   phEnvironment = env } )

convertFiles :: (String, StringObject) -> Either ErrorMessage (String, [FilePath])
convertFiles (name, object) = do
  files <- mapM getString =<< getSequence object
  return (name, files)

