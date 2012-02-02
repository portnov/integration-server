{-# LANGUAGE ExistentialQuantification #-}
module THIS.Protocols.Types where

import Control.Applicative
import Data.Object
import Data.Object.Yaml
import Data.Conduit

import THIS.Types
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

data AnyProtocol = forall p. Protocol p => AnyProtocol p

class (Protocol p) => CommandProtocol p where
  changeWorkingDirectory :: p -> FilePath -> IO ()
  runCommands :: p -> [String] -> IO (Source IO String)

data AnyCommandProtocol =
  forall p. CommandProtocol p => AnyCommandProtocol p

class (Protocol p) => SendProtocol p where
  sendFile :: p -> FilePath -> FilePath -> IO ()
  makeRemoteDirectory :: p -> FilePath -> IO ()

  sendTree :: p -> FilePath -> FilePath -> IO ()

data AnySendProtocol =
  forall p. SendProtocol p => AnySendProtocol p

class (Protocol p) => ReceiveProtocol p where
  receiveFile :: p -> FilePath -> FilePath -> IO ()
  receiveTree :: p -> FilePath -> FilePath -> IO ()

data AnyReceiveProtocol =
  forall p. ReceiveProtocol p => AnyReceiveProtocol p

