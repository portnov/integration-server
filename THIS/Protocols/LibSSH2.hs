{-# LANGUAGE TypeFamilies #-}
module THIS.Protocols.LibSSH2 where

import Control.Applicative
import Network
import Network.Socket hiding (connect)
import System.IO
import Codec.Binary.UTF8.String

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

import THIS.Types
import THIS.Yaml
import THIS.Protocols

data LibSSH2 = LibSSH2 Session

instance Protocol LibSSH2 where
  initializeProtocol _ = do
      initialize True
      return ()

  deinitializeProtocol _ = exit
  
  connect cfg = do
    sock <- socketConnect (cHost cfg) (cPort cfg)
    handle <- socketToHandle sock ReadWriteMode
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
    return (LibSSH2 session)

  disconnect (LibSSH2 session) = do
    disconnectSession session "Done."
    freeSession session
    return ()

instance CommandProtocol LibSSH2 where
  runCommand (LibSSH2 session) command = do
    withChannel session $ \ch -> do
        channelShell ch command
        result <- readAllChannel ch
        let result' = decodeString result
        return result'

instance SendProtocol LibSSH2 where
  sendFile (LibSSH2 session) local remote = do
    sz <- scpSendFile session 0o644 local remote
    return ()

  makeRemoteDirectory (LibSSH2 session) path = do
    (rc,output) <- withChannel session $ \ch -> do
                     channelShell ch $ "mkdir " ++ path
                     readAllChannel ch
    if rc /= 0
      then fail $ "Cannot make remote dir " ++ path ++ ", error " ++ show rc
      else return ()

  sendTree _ _ _ = fail "sendTree: Not implemented"
    
instance ReceiveProtocol LibSSH2 where
  receiveFile (LibSSH2 session) remote local = do
    sz <- scpReceiveFile session remote local
    return ()

  receiveTree _ _ _ = fail "receiveTree: Not implemented"

