{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
-- | libssh2 protocol. Uses libssh2 to send commands
-- via SSH and files via SCP.
module THIS.Protocols.LibSSH2
  (LibSSH2 (..)
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.Generics
import Data.List
import System.IO
import System.Directory
import System.FilePath
-- import Codec.Binary.UTF8.String
import Text.Printf

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2
import Network.SSH.Client.LibSSH2.Conduit

import THIS.Types
import THIS.Util
import THIS.Protocols.Types

data LibSSH2 = LibSSH2 Session (TMVar FilePath) Variables
  deriving (Typeable)

instance Protocol LibSSH2 where
  initializeProtocol _ = do
      initialize True
      return ()

  deinitializeProtocol _ = exit
  
  connect cfg = do
    sock <- socketConnect (cHost cfg) (cPort cfg)
    session <- initSession
    handshake session sock
    checkHost session
        (cHost cfg)
        (cPort cfg)
        (cKnownHosts cfg)
    publicKeyAuthFile session
        (cUsername cfg)
        (cPublicKey cfg)
        (cPrivateKey cfg)
        ""
    -- setTraceMode session [T_TRANS, T_CONN, T_SOCKET, T_ERROR]
    var <- newEmptyTMVarIO
    return (LibSSH2 session var (cEnvironment cfg))

  disconnect (LibSSH2 session _ _) = do
    disconnectSession session "Done."
    freeSession session
    return ()

instance CommandProtocol LibSSH2 where
  data RCHandle LibSSH2 = CH CommandsHandle

  runCommands conn@(LibSSH2 session var vars) commands = do
      mbd <- atomically $ tryTakeTMVar var
      let environment = [printf "export %s=%s" k v | (k, v) <- vars]
      tmp <- tempFile
      writeFile tmp $ unlines $ "#!/bin/bash" : (environment ++ commands)
      let remoteScript = case mbd of
                            Nothing -> "this.sh"
                            Just dir -> dir </> "this.sh"
      sendFile conn tmp remoteScript
      removeFile tmp
      let cmd = "/bin/bash " ++ remoteScript
          cmd' = case mbd of
                   Nothing -> cmd
                   Just dir -> inDir dir cmd
      putStrLn $ "EXEC: " ++ cmd'
      res <- execCommand True session cmd'
      let Just h = fst res
      return (CH h, snd res)
    where
      inDir dir cmd = printf "if cd %s; then %s; else exit 10; fi" dir cmd
  
  getExitStatus (CH h) = getReturnCode h

  changeWorkingDirectory (LibSSH2 _ var _) dir =
      atomically $ putTMVar var dir

instance FilesProtocol LibSSH2 where
  sendFile (LibSSH2 session _ _) local remote = do
    sz <- scpSendFile session 0o644 local remote
    return ()

  makeRemoteDirectory (LibSSH2 session _ _) path = do
    (rc,output) <- withChannel session $ \ch -> do
                     channelExecute ch $ "mkdir " ++ path
                     readAllChannel ch
    if rc /= 0
      then fail $ "Cannot make remote dir " ++ path ++ ", error " ++ show rc
      else return ()

  sendTree _ _ _ = fail "sendTree: Not implemented"
    
  receiveFile (LibSSH2 session _ _) remote local = do
    sz <- scpReceiveFile session remote local
    return ()

  receiveTree _ _ _ = fail "receiveTree: Not implemented"

