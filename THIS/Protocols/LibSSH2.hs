{-# LANGUAGE TypeFamilies #-}
module THIS.Protocols.LibSSH2 where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.Monoid
import Data.Conduit
import Network
import Network.Socket hiding (connect)
import System.IO
import Codec.Binary.UTF8.String
import Text.Printf

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2
import Network.SSH.Client.LibSSH2.Conduit

import THIS.Types
import THIS.Yaml
import THIS.Protocols.Types

data LibSSH2 = LibSSH2 Session (TMVar FilePath)

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
    return (LibSSH2 session var)

  disconnect (LibSSH2 session _) = do
    disconnectSession session "Done."
    freeSession session
    return ()

instance CommandProtocol LibSSH2 where
  data RCHandle LibSSH2 = CH CommandsHandle

  runCommands (LibSSH2 session var) commands = do
      mbd <- atomically $ tryTakeTMVar var
      let cmds = case mbd of
                   Nothing -> commands
                   Just dir -> map (inDir dir) commands
      let bs = replicate (length commands - 1) False ++ [True]
      res <- zipWithM (\cmd b -> execCommand b session cmd) cmds bs
      let Just h = fst (last res)
      return (CH h, mconcat (map snd res))
    where
      inDir dir cmd = printf "if cd %s; then %s; else exit 1; fi" dir cmd
  
  getExitStatus (CH h) = getReturnCode h

  changeWorkingDirectory (LibSSH2 _ var) dir =
      atomically $ putTMVar var dir

instance SendProtocol LibSSH2 where
  sendFile (LibSSH2 session _) local remote = do
    sz <- scpSendFile session 0o644 local remote
    return ()

  makeRemoteDirectory (LibSSH2 session _) path = do
    (rc,output) <- withChannel session $ \ch -> do
                     channelExecute ch $ "mkdir " ++ path
                     readAllChannel ch
    if rc /= 0
      then fail $ "Cannot make remote dir " ++ path ++ ", error " ++ show rc
      else return ()

  sendTree _ _ _ = fail "sendTree: Not implemented"
    
instance ReceiveProtocol LibSSH2 where
  receiveFile (LibSSH2 session _) remote local = do
    sz <- scpReceiveFile session remote local
    return ()

  receiveTree _ _ _ = fail "receiveTree: Not implemented"

