
import Control.Monad
import Control.Monad.Error
import Data.Maybe
import System.Environment
import System.Console.GetOpt

import THIS.Types
import THIS.Yaml
import THIS.Config.Global
import THIS.Config.Parser
import THIS.Execute

data Option =
    Variable String String
  | Help
  deriving (Eq, Show)

usage :: String
usage = usageInfo header options
  where
    header = "Usage: this [OPTIONS] project phase"

options :: [OptDescr Option]
options = [
  Option "e" [] (ReqArg variable "VARIABLE=VALUE") "set environment variable",
  Option "h" ["help"] (NoArg Help) "display this help and exit" ]

variable str =
  let (h, t) = break (== '=') str
  in  Variable h (tail t)

parseCmdLine :: [String] -> ([Option], String, String)
parseCmdLine args =
  case getOpt Permute options args of
    (opts, [pr,ph], []) -> (opts, pr, ph)
    (_, x, []) -> error $ "Unsupported command line arguments.\n" ++ usage
    (_, _, msgs) -> error $ concat msgs ++ usage

getVars :: [Option] -> Variables
getVars = concatMap go
  where
    go (Variable name value) = [(name, value)]
    go _ = []

main :: IO ()
main = do
  args <- getArgs
  let (opts, project, phase) = parseCmdLine args
  if Help `elem` opts
    then putStrLn usage
    else do
         x <- runErrorT $ worker project phase (getVars opts)
         case x of
           Right _ -> putStrLn "Done."
           Left err -> putStrLn $ "Error: " ++ show err

worker :: String -> String -> Variables -> THIS ()
worker project phase vars = do
  gc <- loadGlobalConfig
  execute gc project phase vars

