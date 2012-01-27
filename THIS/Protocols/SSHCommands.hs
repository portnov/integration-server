{-# LANGUAGE TypeFamilies #-}
module THIS.Protocols.SSHCommands where

import Control.Applicative
import System.Process
import System.Exit
import Text.Printf

import THIS.Types
import THIS.Yaml
import THIS.Protocols

data SSHCommands = SSHCommands SSHCommandsConnection

data SSHCommandsConnection = SSHCommandsConnection {
  cHost :: String,
  cPort :: Int,
  cUsername :: String }
  deriving (Eq)

instance Show SSHCommandsConnection where
  show cfg = cUsername cfg ++ "@" ++ cHost cfg ++ ":" ++ show (cPort cfg)

instance Protocol SSHCommands where
  type ConnectionInfo SSHCommands = SSHCommandsConnection

  loadConnectionInfo object = SSHCommandsConnection
      <$> get "host" object
      <*> getOptional "port" 22 object
      <*> getOptional "login" "this" object

  initializeProtocol _ = return ()
  deinitializeProtocol _ = return ()

  connect cfg = return (SSHCommands cfg)
  disconnect _ = return ()

rc2int :: ExitCode -> Int
rc2int ExitSuccess = 0
rc2int (ExitFailure n) = n

runSSH :: SSHCommandsConnection -> [String] -> IO (Int, String)
runSSH cfg params = do
    (ec, out, _) <- readProcessWithExitCode "ssh"
                       (show cfg: params)
                       ""
    return (rc2int ec, out)

instance CommandProtocol SSHCommands where
  runCommand (SSHCommands cfg) command =
    runSSH cfg [command]

instance SendProtocol SSHCommands where
  sendFile (SSHCommands cfg) local remote = do
    let command = printf "scp %s %s:%s" local (show cfg) remote
    ec <- system command
    case ec of
      ExitSuccess -> return ()
      ExitFailure n -> fail $ printf "SCP: %s: error: %s" command (show n)

  makeRemoteDirectory (SSHCommands cfg) path = do
    runSSH cfg ["mkdir", path]
    return ()

  sendTree (SSHCommands cfg) local remote = do
    let command = printf "scp -r %s %s:%s" local (show cfg) remote
    ec <- system command
    case ec of
      ExitSuccess -> return ()
      ExitFailure n -> fail $ printf "SCP: %s: error: %s" command (show n)

instance ReceiveProtocol SSHCommands where
  receiveFile (SSHCommands cfg) remote local = do
    let command = printf "scp %s:%s %s" (show cfg) remote local
    ec <- system command
    case ec of
      ExitSuccess -> return ()
      ExitFailure n -> fail $ printf "SCP: %s: error: %s" command (show n)

  receiveTree (SSHCommands cfg) remote local = do
    let command = printf "scp -r %s:%s %s" (show cfg) remote local
    ec <- system command
    case ec of
      ExitSuccess -> return ()
      ExitFailure n -> fail $ printf "SCP: %s: error: %s" command (show n)

