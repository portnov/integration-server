{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Types where

import Control.Monad.Error
import Control.Failure

data ProjectConfig = ProjectConfig {
    pcDirectory :: FilePath,
    pcHosts :: [(String, HostConfig)],
    pcPhases :: [(String, Phase)],
    pcEnvironment :: [(String, String)] }
  deriving (Eq, Show)

data HostConfig = HostConfig {
     hcHostname :: String,
     hcPath :: FilePath,
     hcVM :: Maybe VMConfig }
  deriving (Eq, Show)

data VMConfig = VMConfig {
      vmHypervisor :: String,
      vmEmpty :: Bool,
      vmTemplatePath :: FilePath,
      vmName :: String,
      vmSnapshot :: String,
      vmParams :: [(String, String)] }
  deriving (Eq, Show)

data Phase = Phase {
    phWhere :: HostConfig,
    phShutdownVM :: Bool,
    phPreExecute :: [String],
    phExecutor :: String,
    phActions :: [String],
    phParser :: String,
    phFiles :: [(String, [FilePath])],
    phShellCommands :: [String],
    phEnvironment :: [(String, String)] }
  deriving (Eq, Show)

data Executor = Executor {
    exActions :: [String],
    exConfigs :: [(String, ActionConfig)] }
  deriving (Eq, Show)

data ActionConfig = ActionConfig {
    acCommands :: [String] }
  deriving (Eq, Show)

type YamlError = String

type YamlM a = ErrorT YamlError IO a

instance Failure e (Either e) where
  failure e = Left e

instance (Monad m) => Failure YamlError (ErrorT YamlError m) where
  failure e = fail e

class RemoteProtocol p where
  connectRemote :: String -> IO p
  disconnectRemote :: p -> IO ()
  runCommand :: p -> String -> IO (Int, String)
  readRemoteFile :: p -> FilePath -> IO String
  writeRemoteFile :: p -> FilePath -> String -> IO ()

