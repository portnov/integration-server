{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
module THIS.Protocols.Types where

import Control.Applicative
import Data.Object
import Data.Object.Yaml
import Data.Conduit
import System.Directory

import THIS.Types
import THIS.Util
import THIS.Yaml

data ConnectionInfo = ConnectionInfo {
  cHost :: String,
  cPort :: Int,
  cUsername :: String,
  cKnownHosts :: FilePath,
  cPublicKey :: FilePath,
  cPrivateKey :: FilePath }
  deriving (Eq)

instance Show ConnectionInfo where
  show cfg = cUsername cfg ++ "@" ++ cHost cfg ++ ":" ++ show (cPort cfg)

class Protocol p where
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

