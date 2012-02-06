-- | Actions output parsing
module THIS.Parse
  (getParserSink,
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
import THIS.Util
import THIS.Yaml
import THIS.Config.Parser
import THIS.Database

data ParserState = ParserState {
    psCurrentGroup :: Maybe ResultGroup -- ^ Current group
  , psGroupName :: String               -- ^ Current group name
  , psParams :: [(String, String)]      -- ^ Current group parameters
  , psLineNr :: Int
  , psOtherLines :: [String]
  } deriving (Eq, Show)

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

-- | Turn list of regexp captures names into infinite list.
-- E.g., [\"a\", \"b\", "\c"\] --> [\"a\", \"b\", \"c\", \"4\", \"5\" ...
continueNames :: [String] -> [String]
continueNames list = list ++ drop (length list) (map show [1..])

-- | Match output line with starting regexps of defined groups.
-- Returns (group name, group params).
matchR :: String                       -- ^ Line to check
       -> [(String, String, [String])] -- ^ [(group name, starting regexp, regexp captures names)]
       -> Maybe (String, Variables)
matchR _ [] = Nothing
matchR line ((name, regex, captures):other) =
  let allMatches = line =~ regex :: [[String]]
  in case allMatches of
       [] -> matchR line other
       [list] -> let names = continueNames captures
                     pairs = zip names (tail list)
                 in  Just (name, pairs)
       _ -> error $ "Unexpected regex result: " ++ show allMatches

-- | Parser itself
parse :: ActionParser -> Conduit String IO ParserResult
parse ap = conduitState emptyState push close
  where
    pairs = apGroups ap

    close _ = return []

    push st line = do
      liftIO $ putStrLn $ "> " ++ line
      let ms = [(name, fst $ head (rgLines g), snd $ head (rgLines g)) | (name, g) <- pairs] 
      case matchR line ms of
        Just (group, params) -> do
          liftIO $ putStrLn $ "~ " ++ group
          let new = step $ addParams params $ setCurrentGroup group (lookup group pairs) st
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
                                 liftIO $ putStrLn $ "~ " ++ regex
                                 let names = continueNames captures
                                     pairs = zip names (tail list)
                                 return $ StateProducing (addParams pairs st) []

-- | Parsing Sink. Puts results to DB.
getParserSink :: DBConfig
              -> ActionRecordId
              -> Parser
              -> String         -- ^ Action name
              -> Either ErrorMessage (ActionParser, Sink ParserResult IO String)
getParserSink dbc arid (Parser parser) action =
  case lookup action parser `mplus` lookup "$$" parser of
    Nothing -> failure $ "Action is not supported by parser: " ++ action
    Just ap -> return (ap, sinkState "ok" (push ap) close)
  where
    push ap st pr = do
      let cur = lookupGroup (apResultsMap ap) (groupName pr)
          new = maximumBy (cmpOrder ap) [st, cur]
      liftIO $ putStrLn $ "result: " ++ new
      liftIO $ runDB dbc $ logOutput arid pr
      return $ StateProcessing new

    close st = return st

-- | Compare results with respect to defined order
cmpOrder :: ActionParser -> String -> String -> Ordering
cmpOrder ap x y = fromMaybe (compare x y) $ do
                 i <- findIndex (== x) (full ap)
                 j <- findIndex (== y) (full ap)
                 return $ compare i j
  where 
    full ap = completeResultsList (map fst $ apResultsMap ap)

-- | Complete list of possible results.
-- Add `ok', `warning' and `error' to list.
completeResultsList :: [String] -> [String]
completeResultsList list =
  ["ok" | "ok" `notElem` list]
  ++ list
  ++ ["warning" | "warning" `notElem` list]
  ++ ["error" | "error" `notElem` list]

-- | Look up for result by output group name
lookupGroup :: [(String, ResultsRange)] -- ^ [(group name, matched results range)]
            -> String                   -- ^ Action result
            -> String
lookupGroup [] _ = "ok"
lookupGroup ((result, range):other) group
    | group `matches` range = result
    | otherwise             = lookupGroup other group

-- | Check if result matches results range
matches :: String -> ResultsRange -> Bool
matches group (ResultsList list) = any good list
  where good (GroupName name) = (name == group)
        good _                = False
matches group _ = False

-- | Update result using exit code
updateResult :: ActionParser
             -> Int          -- ^ Exit code
             -> String       -- ^ Previous result
             -> String
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

