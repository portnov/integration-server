{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
-- | SSH commands protocol. Uses ssh, scp commands
-- to send commands and files.
module THIS.Protocols.SSHCommands
  (SSHCommands (..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import System.Process
import System.Exit
import Text.Printf
import Data.Conduit
import Data.Generics

import THIS.Types
import THIS.Util
import THIS.Yaml
import THIS.Protocols.Types

data SSHCommands = SSHCommands ConnectionInfo
  deriving (Data, Typeable)

instance Protocol SSHCommands where
  initializeProtocol _ = return ()
  deinitializeProtocol _ = return ()

  connect cfg = return (SSHCommands cfg)
  disconnect _ = return ()

runSSH :: ConnectionInfo -> [String] -> IO (Int, String)
runSSH cfg params = do
    (ec, out, _) <- readProcessWithExitCode "ssh"
                       (show cfg: params)
                       ""
    return (rc2int ec, out)

instance CommandProtocol SSHCommands where
  data RCHandle SSHCommands = V (TVar Int)

  runCommands (SSHCommands cfg) commands = do
      var <- newTVarIO 1
      return (V var, sourceState commands (pull var))
    where
      pull _ [] = return StateClosed
      pull var (cmd:other) = do
        (rc, out) <- liftIO $ runSSH cfg [cmd]
        liftIO $ atomically $ writeTVar var rc
        return $ StateOpen other out

  getExitStatus (V var) = atomically $ readTVar var

  changeWorkingDirectory _ _ = fail "chdir not implemented"

instance FilesProtocol SSHCommands where
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

