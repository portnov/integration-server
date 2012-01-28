{-# LANGUAGE TypeFamilies #-}
module THIS.Protocols.SSHCommands where

import Control.Applicative
import Control.Monad
import System.Process
import System.Exit
import Text.Printf

import THIS.Types
import THIS.Yaml
import THIS.Protocols.Types

data SSHCommands = SSHCommands ConnectionInfo

instance Protocol SSHCommands where
  initializeProtocol _ = return ()
  deinitializeProtocol _ = return ()

  connect cfg = return (SSHCommands cfg)
  disconnect _ = return ()

rc2int :: ExitCode -> Int
rc2int ExitSuccess = 0
rc2int (ExitFailure n) = n

runSSH :: ConnectionInfo -> [String] -> IO (Int, String)
runSSH cfg params = do
    (ec, out, _) <- readProcessWithExitCode "ssh"
                       (show cfg: params)
                       ""
    return (rc2int ec, out)

instance CommandProtocol SSHCommands where
  runCommands (SSHCommands cfg) commands = do
    outs <- forM commands $ \cmd -> runSSH cfg [cmd]
    return (fst (last outs), map snd outs)

  changeWorkingDirectory _ _ = fail "chdir not implemented"

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

