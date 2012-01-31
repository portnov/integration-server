{-# LANGUAGE OverloadedStrings #-}
module THIS.Execute where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Failure
import Data.Object
import System.FilePath
import System.FilePath.Glob
import System.Directory

import THIS.Types
import THIS.Yaml
import THIS.Config.ProjectConfig
import THIS.Config.Executor
import THIS.Config.Parser
import THIS.ConnectionsManager
import THIS.Protocols
import THIS.Templates.Text
import THIS.Hypervisor
import THIS.Parse
import THIS.Database

actionCommands :: String -> [(String, String)] -> Executor -> Either String [String]
actionCommands action pairs exe =
  case lookupAction action exe of
    Nothing -> Left $ "Unknown action in executor: " ++ action
    Just ac -> Right (acCommands ac)

environment :: ProjectConfig -> Phase -> [(String, String)]
environment pc ph = phEnvironment ph ++ hcParams (phWhere ph) ++ pcEnvironment pc

execute :: GlobalConfig -> String -> String -> [(String, String)] -> THIS ()
execute gc projectName phase extVars = do
  chosts <- loadCommonHosts
  (ppath, object, pc) <- loadProjectConfig projectName extVars chosts
  let dbc = gcDatabase gc
  pid <- runDB dbc $ checkProject ppath projectName pc
  liftIO $ putStrLn $ "Project ID: " ++ show pid
  case lookup phase (pcPhases pc) of
    Nothing -> lift $ putStrLn $ "No such phase: " ++ phase
    Just ph -> do
      let host = phWhere ph
      liftIO $ putStrLn $ "Executing " ++ phase ++ " on " ++ hcHostname host
      case hcVM host of
        Nothing -> return ()
        Just vm -> do
                   liftIO $ putStrLn $ "Running VM"
                   liftIO $ runVM object (hcParams host) vm
      parser <- loadParser (phParser ph)
      manageConnections (hcParams host) $ do
          when (not $ null $ phCreateFiles ph) $ do
            send <- getSendProtocol (hcHostname host)
            lift $ forM_ (phCreateFiles ph) $ \(template, path) -> do
                      temp <- evalTextFile (Mapping []) (phEnvironment ph) template
                      liftIO $ putStrLn $ "Sending file: " ++ path
                      lift $ sendFileA send temp (hcPath host </> path)
          (exePath, exe) <- lift $ loadExecutor (phExecutor ph)
          let aclist = if null (phActions ph)
                         then if null (exActions exe)
                                then [phase]
                                else exActions exe
                         else phActions ph
          liftIO $ print (hcParams host)
          cmdP <- getCommandProtocol (hcHostname host)
          liftIO $ chdirA cmdP (hcPath host)
          forM_ aclist $ \action -> do
            case lookupAction action exe of
              Nothing -> liftIO $ putStrLn $ "Action is not supported by executor: " ++ action
              Just ac -> when (action /= "$$") $ do
                case actionCommands action (environment pc ph) exe of
                  Left err -> liftIO $ putStrLn $ "Error in command for action " ++ action ++ ": " ++ err
                  Right cmds -> do
                                arid <- runDB dbc $ startAction pid phase action
                                let env = ("action", action): phEnvironment ph
                                commands <- case mapM (evalTemplate exePath object env) cmds of
                                              Left err -> failure err :: MTHIS [String]
                                              Right x -> return x
                                liftIO $ putStrLn $ "Executing: " ++ show commands
                                (ec, out) <- liftIO $ runCommandsA cmdP commands
                                liftIO $ putStrLn $ "Output: " ++ show out
                                rs <- case runParser parser action (ec, out) of
                                        Left err -> do
                                                    liftIO $ putStrLn $ "Parser error: " ++ show err
                                                    return "parse-error"
                                        Right (rr, results) -> do
                                                    runDB dbc $ forM_ results $ logOutput arid
                                                    liftIO $ putStrLn $ "Result: " ++ rr
                                                    return rr
                                runDB dbc $ finishAction arid ec rs
      case hcVM host of
        Nothing -> return ()
        Just vm -> when (phShutdownVM ph) $
                       liftIO $ putStrLn "Shutting VM down"

