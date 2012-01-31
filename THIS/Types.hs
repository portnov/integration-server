{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}
module THIS.Types where

import Control.Monad.Error
import Control.Failure

type Variables = [(String, String)]

data GlobalConfig = GlobalConfig {
    gcDatabase :: DBConfig,
    gcSendmail :: String }
  deriving (Eq, Show)

data DBConfig = DBConfig {
    dbcHost :: String,
    dbcPort :: Int,
    dbcDatabase :: String,
    dbcUser :: String,
    dbcPassword :: String }
  deriving (Eq)

instance Show DBConfig where
  show (DBConfig {..}) = host ++ port ++ db ++ user ++ psw
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
     hcHostname :: String,
     hcPath :: FilePath,
     hcVM :: Maybe VMConfig,
     hcParams :: Variables }
  deriving (Eq, Show)

data VMConfig = VMConfig {
      vmHypervisor :: String,
      vmEmpty :: Bool,
      vmTemplatePath :: FilePath,
      vmName :: String,
      vmSnapshot :: String }
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

type YamlError = String

type YamlM a = ErrorT YamlError IO a

instance Failure e (Either e) where
  failure e = Left e

instance (Monad m) => Failure YamlError (ErrorT YamlError m) where
  failure e = fail e

