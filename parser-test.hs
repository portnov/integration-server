
import Control.Monad
import Control.Monad.Error
import Data.Maybe

import THIS.Types
import THIS.Yaml
import THIS.Config.Parser
import THIS.Parse

main = do
  Right parser <- runErrorT $ loadParser "ghc"
  text <- readFile "ghc-output.txt"
  let ls = lines text
      Right (rr, results) = runParser parser "build" (1, ls)
  forM_ results $ \result -> do
    putStrLn $ "Group: " ++ prGroupName result
    forM_ (prParams result) $ \(key, value) ->
      putStrLn $ "  " ++ key ++ ": " ++ value
    forM_ (prOtherLines result) putStrLn
    putStrLn ""
  putStrLn $ "Result: " ++ rr

