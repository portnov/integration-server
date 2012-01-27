
module THIS.Execute where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import System.FilePath
import System.FilePath.Glob
import System.Directory

import THIS.Types
import THIS.Yaml
import THIS.Config.ProjectConfig
import THIS.Config.Executor
import THIS.ConnectionsManager

actionCommands :: String -> [(String, String)] -> Executor -> Either String [String]
actionCommands action pairs exe =
  case lookupAction action exe of
    Nothing -> Left $ "Unknown action in executor: " ++ action
    Just ac -> mapM (eval action pairs) (acCommands ac)

environment :: ProjectConfig -> Phase -> [(String, String)]
environment pc ph = phEnvironment ph ++ params ++ pcEnvironment pc
  where
    params = case hcVM (phWhere ph) of
               Nothing -> []
               Just vm -> vmParams vm

execute :: FilePath -> String -> YamlM ()
execute path phase = do
  chosts <- loadCommonHosts
  pc <- loadProjectConfig path chosts
  case lookup phase (pcPhases pc) of
    Nothing -> lift $ putStrLn $ "No such phase: " ++ phase
    Just ph -> do
      let host = phWhere ph
      liftIO $ putStrLn $ "Executing " ++ phase ++ " on " ++ hcHostname host
      case hcVM host of
        Nothing -> return ()
        Just vm -> liftIO $ putStrLn $ "Running VM"
      exe <- loadExecutor (phExecutor ph)
      let aclist = if null (phActions ph)
                     then if null (exActions exe)
                            then [phase]
                            else exActions exe
                     else phActions ph
      forM_ aclist $ \action -> do
        case lookupAction action exe of
          Nothing -> lift $ putStrLn $ "Action is not supported by executor: " ++ action
          Just ac -> when (action /= "$$") $ do
            case actionCommands action (environment pc ph) exe of
              Left err -> lift $ putStrLn $ "Error in command for action " ++ action ++ ": " ++ err
              Right cmds -> forM_ cmds $ \cmd -> 
                                 lift $ putStrLn $ "EXEC: " ++ cmd
      case hcVM host of
        Nothing -> return ()
        Just vm -> when (phShutdownVM ph) $
                       liftIO $ putStrLn "Shutting VM down"

