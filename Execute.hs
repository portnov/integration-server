
module Execute where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Types
import Yaml
import ProjectConfig
import Executor

actionCommand :: String -> [(String, String)] -> Executor -> Either String String
actionCommand action pairs exe =
  case lookupAction action exe of
    Nothing -> Left $ "Unknown action in executor: " ++ action
    Just ac -> eval action pairs (acCommand ac)

environment :: ProjectConfig -> Phase -> [(String, String)]
environment pc ph = phEnvironment ph ++ pcEnvironment pc

execute :: FilePath -> String -> ErrorT YamlError IO ()
execute path phase = do
  pc <- loadProjectConfig path
  case lookup phase (pcPhases pc) of
    Nothing -> lift $ putStrLn $ "No such phase: " ++ phase
    Just ph -> do
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
            case actionCommand action (environment pc ph) exe of
              Left err -> lift $ putStrLn $ "Error in command for action " ++ action ++ ": " ++ err
              Right cmd -> lift $ putStrLn $ "EXEC: " ++ cmd

