
module Types where

data ProjectConfig = ProjectConfig {
    pcDirectory :: FilePath,
    pcHosts :: [(String, HostConfig)],
    pcPhases :: [(String, Phase)] }
  deriving (Eq, Show)

data HostConfig = HostConfig {
     hcHostname :: String,
     hcPath :: FilePath,
     hcVM :: Maybe VMConfig }
  deriving (Eq, Show)

data VMConfig = VMConfig {
      vmEmpty :: Bool,
      vmTemplatePath :: FilePath,
      vmName :: String,
      vmMemory :: Integer,
      vmStorageSize :: Integer,
      vmCDROMImage :: FilePath }
  deriving (Eq, Show)

data Phase = Phase {
    phWhere :: HostConfig,
    phPreExecute :: [String],
    phExecutor :: String,
    phParser :: String,
    phFiles :: [(String, [FilePath])],
    phShellCommands :: [String] }
  deriving (Eq, Show)
