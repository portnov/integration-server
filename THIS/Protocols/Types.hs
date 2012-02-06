{-# LANGUAGE ExistentialQuantification, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module THIS.Protocols.Types where

import Control.Failure
import Control.Monad.State
import Control.Monad.Error
import Data.Generics
import qualified Data.Map as M
import Data.Conduit
import System.Directory

import THIS.Types
import THIS.Util

data ConnectionInfo = ConnectionInfo {
  cHost :: String,
  cPort :: Int,
  cUsername :: String,
  cKnownHosts :: FilePath,
  cPublicKey :: FilePath,
  cPrivateKey :: FilePath }
  deriving (Eq, Data, Typeable)

instance Show ConnectionInfo where
  show cfg = cUsername cfg ++ "@" ++ cHost cfg ++ ":" ++ show (cPort cfg)

class (Typeable p) => Protocol p where
  initializeProtocol :: p -> IO ()
  deinitializeProtocol :: p -> IO ()

  connect :: ConnectionInfo -> IO p
  disconnect :: p -> IO ()

data AnyConnection = forall p. Protocol p => AnyConnection p

class (Protocol p) => CommandProtocol p where
  data RCHandle p
  changeWorkingDirectory :: p -> FilePath -> IO ()
  runCommands :: p -> [String] -> IO (RCHandle p, Source IO String)
  getExitStatus :: RCHandle p -> IO Int

data AnyCommandConnection =
  forall p. CommandProtocol p => AnyCommandConnection p

data AnyRCHandle =
  forall p. CommandProtocol p => AnyRCHandle (RCHandle p)

class (Protocol p) => FilesProtocol p where
  sendFile :: p -> FilePath -> FilePath -> IO ()
  sendTree :: p -> FilePath -> FilePath -> IO ()

  receiveFile :: p -> FilePath -> FilePath -> IO ()
  receiveTree :: p -> FilePath -> FilePath -> IO ()

  transferFile :: p -> FilePath -> p -> FilePath -> IO ()
  transferFile srcp src dstp dst = do
    tmp <- tempFile
    receiveFile srcp src tmp
    sendFile dstp tmp dst
    removeFile tmp

  transferTree :: p -> FilePath -> p -> FilePath -> IO ()
  transferTree srcp src dstp dst = do
    tmp <- tempFile
    receiveTree srcp src tmp
    sendTree dstp tmp dst
    removeDirectoryRecursive tmp

  makeRemoteDirectory :: p -> FilePath -> IO ()

data AnyFilesConnection =
  forall p. FilesProtocol p => AnyFilesConnection p

data TransferConnections =
  forall p. FilesProtocol p => TransferConnections p p

data Connections = Connections {
    commandConnections  :: M.Map String AnyCommandConnection,
    sendConnections     :: M.Map String AnyFilesConnection,
    receiveConnections  :: M.Map String AnyFilesConnection,
    transferConnections :: M.Map (String, String) TransferConnections }

type Managed m a = StateT Connections m a

type MTHIS a = StateT Connections (ErrorT ErrorMessage IO) a

instance (Monad m, Failure e m) => Failure e (StateT Connections m) where
  failure e = lift (failure e)

