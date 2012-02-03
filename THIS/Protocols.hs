module THIS.Protocols where

import Control.Applicative
import Control.Monad.Trans
import Control.Failure
import Data.Object
import Data.Object.Yaml
import Data.Maybe
import Data.Conduit
import System.Directory

import THIS.Types
import THIS.Util
import THIS.Yaml
import THIS.Protocols.Types
import THIS.Protocols.Parse
import THIS.Protocols.Manager
import THIS.Protocols.Local
import THIS.Protocols.LibSSH2
import THIS.Protocols.SSHCommands

initializeProtocols :: IO ()
initializeProtocols = do
  initializeProtocol Local
  initializeProtocol (LibSSH2 undefined undefined)
  initializeProtocol (SSHCommands undefined)

deinitializeProtocols :: IO ()
deinitializeProtocols = do
  deinitializeProtocol Local
  deinitializeProtocol (LibSSH2 undefined undefined)
  deinitializeProtocol (SSHCommands undefined)

disconnectA :: AnyConnection -> IO ()
disconnectA (AnyConnection p) = disconnect p

runCommandsA :: AnyCommandConnection -> [String] -> IO (AnyRCHandle, Source IO String)
runCommandsA (AnyCommandConnection p) commands = do
  (h, r) <- runCommands p commands
  return (AnyRCHandle h, r)

getExitStatusA :: AnyRCHandle -> IO Int
getExitStatusA (AnyRCHandle h) = getExitStatus h

chdirA :: AnyCommandConnection -> FilePath -> IO ()
chdirA (AnyCommandConnection p) dir = changeWorkingDirectory p dir

sendFileA :: AnyFilesConnection -> FilePath -> FilePath -> IO ()
sendFileA (AnyFilesConnection p) local remote =
  sendFile p local remote

makeRemoteDirectoryA :: AnyFilesConnection -> FilePath -> IO ()
makeRemoteDirectoryA (AnyFilesConnection p) path =
  makeRemoteDirectory p path

sendTreeA :: AnyFilesConnection -> FilePath -> FilePath -> IO ()
sendTreeA (AnyFilesConnection p) local remote =
  sendTree p local remote

receiveFileA :: AnyFilesConnection -> FilePath -> FilePath -> IO ()
receiveFileA (AnyFilesConnection p) remote local =
  receiveFile p remote local

receiveTreeA :: AnyFilesConnection -> FilePath -> FilePath -> IO ()
receiveTreeA (AnyFilesConnection p) remote local =
  receiveTree p remote local

transferFiles :: HostConfig -> FilePath -> HostConfig -> FilePath -> MTHIS ()
transferFiles srchost src dsthost dst = do
  let isDir = last src == '/'
  mbtc <- getTransferConnections srchost dsthost
  case mbtc of
    Nothing -> do
        tmp <- liftIO tempFile
        recvP <- getReceiveConnection srchost
        sendP <- getSendConnection    dsthost
        if isDir
          then liftIO $ do
               receiveTreeA recvP src tmp
               sendTreeA    sendP tmp dst
               removeDirectoryRecursive tmp
          else liftIO $ do
               receiveFileA recvP src tmp
               sendFileA    sendP tmp dst
               removeFile tmp
    Just (TransferConnections srcp dstp) -> do
         if isDir
           then liftIO $ transferTree srcp src dstp dst
           else liftIO $ transferFile srcp src dstp dst

