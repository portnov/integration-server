{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RecordWildCards, DeriveDataTypeable, FlexibleContexts, TypeSynonymInstances #-}
module THIS.Types where

import Control.Monad.Error as E
import Control.Failure
import Data.Object.Yaml
import qualified Text.Parsec as P

type Variables = [(String, String)]

data GlobalConfig = GlobalConfig {
    gcDatabase :: DBConfig,
    gcSendmail :: String,
    gcMailFrom :: String }
  deriving (Eq, Show)

data DBConfig = DBConfig {
    dbcHost :: String,
    dbcPort :: Int,
    dbcDatabase :: String,
    dbcUser :: String,
    dbcPassword :: String }
  deriving (Eq)

instance Show DBConfig where
  show (DBConfig {..}) =
      unwords [host, port, db, user, psw]
    where
      host | null dbcHost = ""
           | otherwise    = "host=" ++ dbcHost

      port | dbcPort == 0 = ""
           | otherwise    = "port=" ++ show dbcPort

      db | null dbcDatabase = ""
         | otherwise        = "dbname=" ++ dbcDatabase

      user | null dbcUser = ""
           | otherwise    = "user=" ++ dbcUser

      psw | null dbcPassword = ""
          | otherwise        = "password=" ++ dbcPassword

data ProjectConfig = ProjectConfig {
    pcDirectory :: FilePath,
    pcTitle :: String,
    pcOwner :: String,
    pcHosts :: [(String, HostConfig)],
    pcPhases :: [(String, Phase)],
    pcEnvironment :: Variables }
  deriving (Eq, Show)

data HostConfig = HostConfig {
     hcName :: String,
     hcHostname :: String,
     hcPath :: FilePath,
     hcVM :: Maybe VMConfig,
     hcCommandsProtocol :: String,
     hcSendProtocol :: String,
     hcReceiveProtocol :: String,
     hcParams :: Variables }
  deriving (Eq, Show)

data VMConfig = VMConfig {
      vmHypervisor :: String,
      vmEmpty :: Bool,
      vmTemplatePath :: FilePath,
      vmName :: String,
      vmSnapshot :: String,
      vmStartupTime :: Int }
  deriving (Eq, Show)

data Phase = Phase {
    phWhere :: HostConfig,
    phShutdownVM :: Bool,
    phPreExecute :: [String],
    phExecutor :: String,
    phActions :: [String],
    phParser :: String,
    phCreateFiles :: [(FilePath, FilePath)],
    phFiles :: [(String, [FilePath])],
    phShellCommands :: [String],
    phEnvironment :: Variables }
  deriving (Eq, Show)

data Executor = Executor {
    exActions :: [String],
    exConfigs :: [(String, ActionConfig)] }
  deriving (Eq, Show)

data ActionConfig = ActionConfig {
    acCommands :: [String] }
  deriving (Eq, Show)

data Parser = Parser [(String, ActionParser)]
  deriving (Eq, Show)

data ActionParser = ActionParser {
    apResultsMap :: [(String, ResultsRange)],
    apGroups :: [(String, ResultGroup)] }
  deriving (Eq, Show)

data ResultsRange =
    ResultsList [ResultVariant]
  | CodesRange Int Int
  deriving (Eq, Show)

data ResultVariant = ReturnCode Int | GroupName String
  deriving (Eq, Show)

data ResultGroup = ResultGroup { rgLines :: [(String, [String])] }
  deriving (Eq, Show)

data ErrorMessage =
    YamlError ParseException
  | ParsecError P.ParseError
  | Message String
  | NoMessage
  deriving (Show)

data ParserResult = ParserResult {
  prGroupName :: String,
  prParams :: Variables,
  prOtherLines :: [String] }
  deriving (Eq, Show)

instance E.Error ErrorMessage where
  noMsg = NoMessage
  strMsg = Message

type THIS a = ErrorT ErrorMessage IO a

instance Failure e (Either e) where
  failure e = Left e

instance Failure String (Either ErrorMessage) where
  failure e = Left (Message e)

instance Failure ParseException (Either ErrorMessage) where
  failure e = Left (YamlError e)

instance Failure P.ParseError (Either ErrorMessage) where
  failure e = Left (ParsecError e)

instance (Monad m) => Failure ErrorMessage (ErrorT ErrorMessage m) where
  failure e = ErrorT (return $ Left e)

instance (Monad m) => Failure String (ErrorT ErrorMessage m) where
  failure e = ErrorT (return $ Left $ Message e)

instance (Monad m) => Failure ParseException (ErrorT ErrorMessage m) where
  failure e = ErrorT (return $ Left $ YamlError e)

instance (Monad m) => Failure P.ParseError (ErrorT ErrorMessage m) where
  failure e = ErrorT (return $ Left $ ParsecError e)

