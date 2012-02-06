-- | Hosts communications protocols
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

-- | Initialize all protocols
initializeProtocols :: IO ()
initializeProtocols = do
  initializeProtocol Local
  initializeProtocol (LibSSH2 undefined undefined)
  initializeProtocol (SSHCommands undefined)

-- | Deinitialize all protocols
deinitializeProtocols :: IO ()
deinitializeProtocols = do
  deinitializeProtocol Local
  deinitializeProtocol (LibSSH2 undefined undefined)
  deinitializeProtocol (SSHCommands undefined)

-- | Disconnect any connection
disconnectA :: AnyConnection -> IO ()
disconnectA (AnyConnection p) = disconnect p

-- | Run commands on remote host.
-- Returns (retun code handle, Source).
runCommandsA :: AnyCommandConnection
             -> [String]                          -- ^ Commands
             -> IO (AnyRCHandle, Source IO String)
runCommandsA (AnyCommandConnection p) commands = do
  (h, r) <- runCommands p commands
  return (AnyRCHandle h, r)

-- | Get command exit status
getExitStatusA :: AnyRCHandle -> IO Int
getExitStatusA (AnyRCHandle h) = getExitStatus h

-- | Change working directory on remote host
chdirA :: AnyCommandConnection -> FilePath -> IO ()
chdirA (AnyCommandConnection p) dir = changeWorkingDirectory p dir

-- | Send file from local to remote host
sendFileA :: AnyFilesConnection
          -> FilePath -- ^ Local file path
          -> FilePath -- ^ Remote file path
          -> IO ()
sendFileA (AnyFilesConnection p) local remote =
  sendFile p local remote

-- | Make directory on remote host
makeRemoteDirectoryA :: AnyFilesConnection -> FilePath -> IO ()
makeRemoteDirectoryA (AnyFilesConnection p) path =
  makeRemoteDirectory p path

-- | Send files tree to remote host
sendTreeA :: AnyFilesConnection
          -> FilePath -- ^ Local directory path
          -> FilePath -- ^ Remote directory path
          -> IO ()
sendTreeA (AnyFilesConnection p) local remote =
  sendTree p local remote

-- | Receive file from remote host
receiveFileA :: AnyFilesConnection
             -> FilePath -- ^ Remote file path
             -> FilePath -- ^ Local file path
             -> IO ()
receiveFileA (AnyFilesConnection p) remote local =
  receiveFile p remote local

-- | Receive files tree from remote host
receiveTreeA :: AnyFilesConnection
             -> FilePath -- ^ Remote directory path
             -> FilePath -- ^ Local directory path
             -> IO ()
receiveTreeA (AnyFilesConnection p) remote local =
  receiveTree p remote local

-- | Transfer files from one remote host to another
transferFiles :: HostConfig -- ^ Source host
              -> FilePath   -- ^ Source path
              -> HostConfig -- ^ Destination host
              -> FilePath   -- ^ Destination path
              -> MTHIS ()
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

