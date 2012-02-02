
module THIS.Parse
  (ParserResult (..),
   getParserSink,
   parse,
   updateResult
  ) where

import Control.Monad
import Control.Monad.State as St
import Control.Failure
import Data.Maybe
import Data.List
import Text.Regex.PCRE
import Data.Conduit

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
  psOtherLines :: [String] }
  deriving (Eq, Show)

emptyState :: ParserState
emptyState = ParserState {
               psCurrentGroup = Nothing,
               psGroupName = "",
               psParams = [],
               psLineNr = 0,
               psOtherLines = [] }

setCurrentGroup :: String -> Maybe ResultGroup -> ParserState -> ParserState
setCurrentGroup name grp st = st {
                    psCurrentGroup = grp,
                    psGroupName = name,
                    psParams = [],
                    psOtherLines = [],
                    psLineNr = 1 }

getResult :: ParserState -> ParserResult
getResult st = ParserResult {
              prGroupName = psGroupName st,
              prParams = psParams st,
              prOtherLines = psOtherLines st }

addParams :: [(String, String)] -> ParserState -> ParserState
addParams params st = st { psParams = psParams st ++ params }

addOtherLine :: String -> ParserState -> ParserState
addOtherLine line st = st { psOtherLines = psOtherLines st ++ [line] }

step :: ParserState -> ParserState
step st = st { psLineNr = psLineNr st + 1 }

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

parse :: ActionParser -> Conduit String IO ParserResult
parse ap = conduitState emptyState push close
  where
    pairs = apGroups ap

    close _ = return []

    push st line = do
      let ms = [(name, fst $ head (rgLines g), snd $ head (rgLines g)) | (name, g) <- pairs] 
      case matchR line ms of
        Just (group, params) -> do
          let new = addParams params $ setCurrentGroup group (lookup group pairs) st
              res = getResult st
          return (StateProducing new [res])

        Nothing -> do
          let i = psLineNr st
              mbg = psCurrentGroup st
          case mbg of
            Nothing -> return (StateProducing st [])
            Just g  -> do
              if i >= length (rgLines g)
                then return $ StateProducing (addOtherLine line st) []
                else do
                     let (regex, captures) = rgLines g !! i
                     case line =~ regex of
                       [] -> return $ StateProducing (addOtherLine line st) []
                       [list] -> do
                                 let names = continueNames captures
                                     pairs = zip names (tail list)
                                 return $ StateProducing (addParams pairs st) []

groupName :: ParserResult -> String
groupName pr =
  case lookup "group" (prParams pr) of
    Just group -> group
    Nothing    -> prGroupName pr

getParserSink :: Parser -> String -> Either ErrorMessage (ActionParser, Sink ParserResult IO String)
getParserSink (Parser parser) action =
  case lookup action parser `mplus` lookup "$$" parser of
    Nothing -> failure $ "Action is not supported by parser: " ++ action
    Just ap -> return (ap, sinkState "ok" (push ap) close)
  where
    push ap st pr = do
      let cur = lookupGroup 0 (apResultsMap ap) (groupName pr)
          new = maximumBy (cmpOrder ap) [st, cur]
      liftIO $ putStrLn $ "result: " ++ new
      return $ StateProcessing new

    close st = return st

cmpOrder :: ActionParser -> String -> String -> Ordering
cmpOrder ap x y = fromMaybe (compare x y) $ do
                 i <- findIndex (== x) (full ap)
                 j <- findIndex (== y) (full ap)
                 return $ compare i j
  where 
    full ap = completeResultsList (map fst $ apResultsMap ap)

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

updateResult :: ActionParser -> Int -> String -> String
updateResult ap rc rr = 
  let groups = apGroups ap
      pairs  = apResultsMap ap
      full   = completeResultsList (map fst pairs)
  in  maximumBy (cmpOrder ap) [lookupCode rc pairs, rr]

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

