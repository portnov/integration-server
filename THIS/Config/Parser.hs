
module THIS.Config.Parser where

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
import THIS.Yaml

loadParser :: FilePath -> YamlM Parser
loadParser name = do
  (_, object) <- loadYaml "parsers" name
  ErrorT (return $ convertParser object)

convertParser :: StringObject -> Either YamlError Parser
convertParser object = Parser <$> (mapM convertAction =<< getMapping object)

convertAction :: (String, StringObject) -> Either YamlError (String, ActionParser)
convertAction (name, object) = do
  results <- mapM convertResults =<< get "result" object
  groups <- mapM convertGroup =<< get "groups" object
  return (name, ActionParser results groups)

convertResults :: (String, StringObject) -> Either YamlError (String, ResultsRange)
convertResults (name, object) = do
    range <- case object of
                  Scalar s -> if all isDigit s
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

    toResult s = if all isDigit s
                   then ReturnCode (read s)
                   else GroupName s

    getRange pairs = do
      min <- lookup "min" pairs
      max <- lookup "max" pairs
      return $ CodesRange (read min) (read max)

convertGroup :: (String, StringObject) -> Either YamlError (String, ResultGroup)
convertGroup (name, object) = do
  group <- mapM convertLine =<< getMapping object
  return (name, ResultGroup group)

convertLine :: (String, StringObject) -> Either YamlError (String, [String])
convertLine (regex, object) = do
  captures <- mapM getString =<< getSequence object
  return (regex, captures)


