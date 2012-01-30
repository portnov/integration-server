
module THIS.Parse where

import Control.Monad
import Control.Monad.State
import Text.Regex.PCRE

import THIS.Types
import THIS.Yaml
import THIS.Config.Parser

parse :: [(String, ResultGroup)] -> [String] -> [(String, [(String, String)]
parse pairs strings = evalState (mapM go strings) emptyState
  where
    go :: String -> State ParseState ()
    go string = do
      let start = matchR string [(name, head (rgLines g)) | (name, g) <- pairs]

matchR :: String -> [(String, String)] -> Maybe (String, [(String, String)])

