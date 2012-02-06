{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Project configs loader
module THIS.Config.ProjectConfig
  (loadProjectConfig,
   loadHost,
   loadCommonHosts,
   usedHosts,
   thisHost
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Failure
import Data.Object
import Data.Object.Yaml
import System.Environment
import System.FilePath
import System.FilePath.Glob

import THIS.Types
import THIS.Util
import THIS.Yaml
import THIS.Templates.Text

-- | Load host configuration by name.
loadHost :: String -> THIS (String, HostConfig)
loadHost path = do
  x <- liftIO (decodeFile path :: IO (Either ParseException StringObject))
  case x of
    Left err -> failure (show err)
    Right object -> liftEither $ convertHost ("", object)

-- | Load common hosts (from \/etc\/this\/hosts and ~\/.config\/this\/hosts).
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

-- | Load project config by name.
-- Returns loaded file path, StringObject and loaded project.
loadProjectConfig :: String                 -- ^ Project name
                  -> Variables              -- ^ External variables
                  -> [(String, HostConfig)] -- ^ Preloaded hosts: [(host name, host config)]
                  -> THIS (FilePath, StringObject, ProjectConfig)
loadProjectConfig name vars hosts = do
  (path, object) <- loadYaml "projects" name
  pc <- ErrorT $ return $ convertProject path vars hosts object
  return (path, object, pc)

-- | List of hosts used by project
usedHosts :: ProjectConfig -> [HostConfig]
usedHosts pc = map (phWhere . snd) (pcPhases pc)

-- | `this' host (localhost).
thisHost :: FilePath  -- ^ Base directory
         -> HostConfig
thisHost dir =
    HostConfig {
      hcName     = "this",
      hcHostname = "localhost",
      hcPath     = dir,
      hcVM       = Nothing,
      hcCommandsProtocol = "local",
      hcSendProtocol     = "local",
      hcReceiveProtocol  = "local",
      hcParams   = [("host", "localhost")] }

convertProject :: FilePath
               -> Variables
               -> [(String, HostConfig)]
               -> StringObject
               -> Either ErrorMessage ProjectConfig
convertProject path vars commonHosts object = do
  dir <- get "directory" object
  title <- getOptional "title" (takeFileName path) object
  owner <- getOptional "owner" "admin" object
  hosts <- mapM convertHost =<< getOptional "hosts" [] object
  let allHosts = [("this", thisHost dir)] ++ commonHosts ++ hosts
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
                      <*> getOptional "snapshot" "" object
                      <*> getOptional "startup-time" 10 object )
            _ -> failure $ "Unknown host type: " ++ ht
  generic <- getOptional "protocol" "libssh2" object
  cmdP    <- getOptional "command-protocol" generic object
  sendP   <- getOptional "send-protocol"    cmdP    object
  recvP   <- getOptional "receive-protocol" cmdP    object
  params <- getPairs object
  return (name, HostConfig {
             hcName = if null name then hostname else name,
             hcHostname = hostname,
             hcPath = path,
             hcVM = mbvm,
             hcParams = params,
             hcCommandsProtocol = cmdP,
             hcSendProtocol = sendP,
             hcReceiveProtocol = recvP } )

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

