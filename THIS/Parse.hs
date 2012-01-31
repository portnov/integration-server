
module THIS.Parse
  (ParserResult (..),
   runParser
  ) where

import Control.Monad
import Control.Monad.State as St
import Control.Failure
import Data.Maybe
import Data.List
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

parse :: [(String, ResultGroup)] -> [String] -> [ParserResult]
parse pairs strings =
    filter (\r -> prGroupName r /= "") $ psResult $ execState (mapM go strings >> saveResults) emptyState
  where
    go :: String -> State ParserState ()
    go line = do
      let ms = [(name, fst $ head (rgLines g), snd $ head (rgLines g)) | (name, g) <- pairs] 
      case matchR line ms of
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

groupName :: ParserResult -> String
groupName pr =
  case lookup "group" (prParams pr) of
    Just group -> group
    Nothing    -> prGroupName pr

runParser :: Parser -> String -> (Int, [String]) -> Either ErrorMessage (String, [ParserResult])
runParser (Parser parser) action (code, output) =
  case lookup action parser `mplus` lookup "$$" parser of
    Nothing -> failure $ "Action is not supported by parser: " ++ action
    Just ap -> let pre = lookupCode code (apResultsMap ap)
                   results = parse (apGroups ap) output
                   groups  = map groupName results
                   res = map (lookupGroup code (apResultsMap ap)) groups
                   maxResult = selectMaxResult (map fst $ apResultsMap ap) res
               in  Right (maxResult, results)

selectMaxResult :: [String] -> [String] -> String
selectMaxResult _ [] = "error"
selectMaxResult base results =
    maximumBy cmpOrder results
  where
    full = completeResultsList base
    cmpOrder x y = fromMaybe (compare x y) $ do
                     i <- findIndex (== x) full
                     j <- findIndex (== y) full
                     return $ compare i j

completeResultsList :: [String] -> [String]
completeResultsList list =
  ["ok" | "ok" `notElem` list]
  ++ list
  ++ ["warning" | "warning" `notElem` list]
  ++ ["error" | "error" `notElem` list]

lookupGroup :: Int -> [(String, ResultsRange)] -> String -> String
lookupGroup 0 [] _ = "ok"
lookupGroup _ [] _ = "error"
lookupGroup c ((result, range):other) group
    | group `matches` range = result
    | otherwise             = lookupGroup c other group

matches :: String -> ResultsRange -> Bool
matches group (ResultsList list) = any good list
  where good (GroupName name) = (name == group)
        good _                = False
matches group _ = False

lookupCode :: Int -> [(String, ResultsRange)] -> String
lookupCode 0 [] = "ok"
lookupCode _ [] = "error"
lookupCode c ((result, range):other)
    | c `inRange` range = result
    | otherwise         = lookupCode c other

inRange :: Int -> ResultsRange -> Bool
inRange c (ResultsList list) = any good list
  where good (ReturnCode r) = (r == c)
        good _              = False
inRange c (CodesRange a b)  = (c >= a) && (c <= b)

