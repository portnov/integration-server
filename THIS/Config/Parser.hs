-- | Parsers loader
module THIS.Config.Parser
  (loadParser
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import Control.Failure
import Data.Maybe
import Data.Char
import Data.Object
import Data.Object.Yaml
import Text.Printf

import THIS.Types
import THIS.Util
import THIS.Yaml

-- | Load parser by name
loadParser :: String -> THIS Parser
loadParser name = do
  (_, object) <- loadYaml "parsers" name
  liftEither $ convertParser object

convertParser :: StringObject -> Either ErrorMessage Parser
convertParser object = Parser <$> (mapM convertAction =<< getMapping object)

convertAction :: (String, StringObject) -> Either ErrorMessage (String, ActionParser)
convertAction (name, object) = do
  results <- mapM convertResults =<< get "result" object
  groups <- mapM convertGroup =<< get "groups" object
  return (name, ActionParser results groups)

convertResults :: (String, StringObject) -> Either ErrorMessage (String, ResultsRange)
convertResults (name, object) = do
    range <- case object of
                  Scalar s -> if (all isDigit s) && (not $ null s)
                                then return $ CodesRange (read s) (read s)
                                else return $ ResultsList [GroupName s]
                  Sequence list -> ResultsList <$> (map toResult <$> mapM getString list)
                  Mapping pairs -> do
                                   pairs' <- mapM secondString pairs
                                   case getRange pairs' of
                                     Nothing -> failure $ "Invalid parser results map: expected min/max, got: " ++ show pairs
                                     Just r -> return r
    return (name, range)
  where
    secondString (name, Scalar s) = return (name, s)
    secondString (name, object) = failure $ "Expected scalar, but got: " ++ show object

    toResult s = if (all isDigit s) && (not $ null s)
                   then ReturnCode (read s)
                   else GroupName s

    getRange pairs = do
      min <- lookup "min" pairs
      max <- lookup "max" pairs
      return $ CodesRange (read min) (read max)

convertGroup :: (String, StringObject) -> Either ErrorMessage (String, ResultGroup)
convertGroup (name, object) = do
  group <- mapM convertLine =<< getMapping object
  return (name, ResultGroup group)

convertLine :: (String, StringObject) -> Either ErrorMessage (String, [String])
convertLine (regex, object) = do
  captures <- mapM getString =<< getSequence object
  return (regex, captures)


