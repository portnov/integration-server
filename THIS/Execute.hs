{-# LANGUAGE OverloadedStrings #-}
module THIS.Execute
  (execute
  ) where

import Control.Monad
import Control.Monad.Error
import Control.Failure
import Data.Object
import System.FilePath
import System.Directory
import Data.Conduit

import THIS.Types
import THIS.Util
import THIS.Config.ProjectConfig
import THIS.Config.Executor
import THIS.Config.Parser
import THIS.Protocols
import THIS.Templates.Text
import THIS.Hypervisor
import THIS.Parse
import THIS.Database
import THIS.Notify

-- | Get list of commands for given action from executor
actionCommands :: String                 -- ^ Action name
               -> Executor
               -> Either String [String]
actionCommands action exe =
  case lookupAction action exe of
    Nothing -> Left $ "Unknown action in executor: " ++ action
    Just ac -> Right (acCommands ac)

-- | Compose environment for given project and phase
environment :: ProjectConfig -> Phase -> Variables -> Variables
environment pc ph ext =
  phEnvironment ph ++ hcParams (phWhere ph) ++ pcEnvironment pc ++ ext

getHost :: String -> [(String, HostConfig)] -> THIS HostConfig
getHost name pairs =
  case lookup name pairs of
    Just hc -> return hc
    Nothing -> failure $ "Host not defined: " ++ name

-- | Execute a phase for the project
execute :: GlobalConfig
        -> String           -- ^ Project name
        -> String           -- ^ Phase name
        -> Variables        -- ^ External environment
        -> THIS ()
execute gc projectName phase extVars = do
  commonHosts <- loadCommonHosts
  (ppath, object, pc) <- loadProjectConfig projectName extVars commonHosts
  let baseEnvironment = [("project", pcTitle pc),
                         ("path",    ppath),
                         ("phase",   phase)]
                        ++ pcEnvironment pc ++ extVars
  baseDir <- liftEither $
                 evalTemplate ppath object baseEnvironment (pcDirectory pc)
  liftIO $ setCurrentDirectory baseDir
  let dbc = gcDatabase gc
  pid <- runDB dbc $ checkProject ppath projectName pc
  case lookup phase (pcPhases pc) of
    Nothing -> lift $ putStrLn $ "No such phase: " ++ phase
    Just ph -> do
      let allHosts = [("this", thisHost baseDir)]
                     ++ [(hcName h, h) | h <- usedHosts pc]
                     ++ commonHosts
      manageConnections (map snd allHosts) $ do
        let host = phWhere ph
        liftIO $ putStrLn $ "Executing " ++ phase ++ " on " ++ hcHostname host
        case hcVM host of
          Nothing -> return ()
          Just vm -> do
                     liftIO $ putStrLn $ "Running VM"
                     liftIO $ runVM object (hcParams host) vm
        (exePath, exe) <- lift $ loadExecutor (phExecutor ph)
        parser <- lift $ loadParser (phParser ph)
        let phaseEnvironment = [("project", pcTitle pc),
                                ("path",    ppath),
                                ("phase",   phase)]
                               ++ environment pc ph extVars
        cmdP <- getCommandConnection host
        hostPath <- lift $ liftEither $
                        evalTemplate ppath object phaseEnvironment (hcPath host)
        liftIO $ chdirA cmdP hostPath
        forM_ (phFiles ph) $ \(srchostname, files) -> do
          srchost <- lift $ getHost srchostname allHosts
          forM_ files $ \path ->
            transferFiles srchost path host path
        createFiles host hostPath (phCreateFiles ph) phaseEnvironment
        result <- executeActions dbc cmdP pid host phase ph exePath exe parser object phaseEnvironment
        let notifyEnvironment = ("result", result): phaseEnvironment
        lift $ sendNotifications gc pid pc object notifyEnvironment phase result
        case hcVM host of
          Nothing -> return ()
          Just vm -> when (phShutdownVM ph) $ do
                         liftIO $ putStrLn "Shutting VM down"
                         liftIO $ shutdownVM vm

-- | Execute all actions for project's phase.
-- Calls executors and parsers.
-- Returns result of last action.
executeActions :: DBConfig
               -> AnyCommandConnection
               -> ProjectId
               -> HostConfig
               -> String       -- ^ Phase name
               -> Phase
               -> FilePath     -- ^ Path to executor description
               -> Executor
               -> Parser
               -> StringObject -- ^ Project config object
               -> Variables    -- ^ Phase environment
               -> MTHIS String
executeActions dbc cmdP pid host phase ph exePath exe parser object phaseEnvironment = do
  -- If actions list is defined for the phase, use it;
  -- Otherwise, use list of actions defined for the executor;
  -- If it isn't defined too, actions list == [phase name]
  let actions = if null (phActions ph)
                 then if null (exActions exe)
                        then [phase]
                        else exActions exe
                 else phActions ph
  res <- forM actions $ \action -> do
    case lookupAction action exe of
      Nothing -> do
        liftIO $ putStrLn $ "Action is not supported by executor: " ++ action
        return "error"
      Just actionConfig ->
        if action == "$$"
          then return "$$"
          else do
            let cmds = acCommands actionConfig
            -- Log action start to DB
            arid <- runDB dbc $ startAction pid phase action
            -- Substitute current environment to the action
            actionRendered <- lift $ liftEither $
                                  evalTemplate exePath object phaseEnvironment action
            -- For commands, add "action" variable to environment
            let commandsEnv = ("action", actionRendered): phaseEnvironment
            commands <- lift $ liftEither $
                            mapM (evalTemplate exePath object commandsEnv) cmds
            liftIO $ putStrLn $ "Executing: " ++ show commands
            -- Get source to read commands output and return code
            (rch, source) <- liftIO $ runCommandsA cmdP commands
            -- Get sink to parse and log commands output
            (actionParser, sink) <- lift $ liftEither $ getParserSink dbc arid parser action
            result <- liftIO $ runResourceT $ source $= parse actionParser $$ sink
            exitCode <- liftIO $ getExitStatusA rch
            let finalResult = updateResult actionParser exitCode result
            liftIO $ putStrLn $ "Exit code: " ++ show exitCode
            -- Log action end in DB
            runDB dbc $ finishAction arid exitCode finalResult
            return finalResult
  return (last res)

-- | Create files by templates
createFiles :: HostConfig
            -> FilePath           -- ^ Current directory on remote host
            -> [(String, String)] -- ^ (template name, file path)
            -> Variables          -- ^ Environment for templates
            -> MTHIS ()
createFiles host remoteBase pairs env = when (not $ null pairs) $ do
    send <- getSendConnection host
    lift $ forM_ pairs $ \(template, path) -> do
              temp <- evalTextFile (Mapping []) env template
              liftIO $ putStrLn $ "Sending file: " ++ path
              lift $ sendFileA send temp (remoteBase </> path)
  
