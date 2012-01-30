
module THIS.Parse where

import Control.Monad
import Control.Monad.State as St
import Text.Regex.PCRE

import THIS.Types
import THIS.Yaml
import THIS.Config.Parser

data ParserResult = ParserResult {
  prGroupName :: String,
  prParams :: Variables,
  prOtherLines :: [String] }
  deriving (Eq, Show)

data ParserState = ParserState {
  psCurrentGroup :: Maybe ResultGroup,
  psGroupName :: String,
  psParams :: [(String, String)],
  psLineNr :: Int,
  psOtherLines :: [String],
  psResult :: [ParserResult] }
  deriving (Eq, Show)

emptyState :: ParserState
emptyState = ParserState {
               psCurrentGroup = Nothing,
               psGroupName = "",
               psParams = [],
               psLineNr = 0,
               psOtherLines = [],
               psResult = [] }

setCurrentGroup :: String -> Maybe ResultGroup -> State ParserState ()
setCurrentGroup name grp =
  modify $ \st -> st {
                    psCurrentGroup = grp,
                    psGroupName = name,
                    psParams = [],
                    psOtherLines = [],
                    psLineNr = 1 }

saveResults :: State ParserState ()
saveResults = do
  st <- St.get
  let new = ParserResult {
              prGroupName = psGroupName st,
              prParams = psParams st,
              prOtherLines = psOtherLines st }
  put $ st { psResult = psResult st ++ [new] }

addParams :: [(String, String)] -> State ParserState ()
addParams params =
  modify $ \st -> st { psParams = psParams st ++ params }

addOtherLine :: String -> State ParserState ()
addOtherLine line =
  modify $ \st -> st { psOtherLines = psOtherLines st ++ [line] }

step :: State ParserState ()
step = modify $ \st -> st { psLineNr = psLineNr st + 1 }

parse :: [(String, ResultGroup)] -> [String] -> [ParserResult]
parse pairs strings = psResult $ execState (mapM go strings) emptyState
  where
    go :: String -> State ParserState ()
    go line = do
      case matchR line [(name, fst $ head (rgLines g), snd $ head (rgLines g)) | (name, g) <- pairs] of
        Just (group, params) -> do
                                saveResults
                                setCurrentGroup group (lookup group pairs)
                                addParams params
        Nothing -> do
                   i <- gets psLineNr
                   mbg <- gets psCurrentGroup
                   case mbg of
                     Nothing -> return ()
                     Just g -> do
                       if i >= length (rgLines g)
                         then addOtherLine line
                         else do
                              let (regex, captures) = rgLines g !! i
                              case line =~ regex of
                                [] -> addOtherLine line
                                [list] -> do
                                          let names = continueNames captures
                                              pairs = zip names (tail list)
                                          addParams pairs

continueNames :: [String] -> [String]
continueNames list = list ++ drop (length list) (map show [1..])

matchR :: String -> [(String, String, [String])] -> Maybe (String, [(String, String)])
matchR _ [] = Nothing
matchR line ((name, regex, captures):other) =
  let allMatches = line =~ regex :: [[String]]
  in case allMatches of
       [] -> matchR line other
       [list] -> let names = continueNames captures
                     pairs = zip names (tail list)
                 in  Just (name, pairs)
       _ -> error $ "Unexpected regex result: " ++ show allMatches

