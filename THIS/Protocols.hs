{-# LANGUAGE TypeFamilies #-}
module THIS.Protocols where

import Data.Object
import Data.Object.Yaml

import THIS.Types

class Protocol p where
  type ConnectionInfo p

  loadConnectionInfo :: StringObject -> Either YamlError (ConnectionInfo p)

  initializeProtocol :: p -> IO ()
  deinitializeProtocol :: p -> IO ()

  connect :: ConnectionInfo p -> IO p
  disconnect :: p -> IO ()

class (Protocol p) => CommandProtocol p where
  runCommand :: p -> String -> IO (Int, String)

class (Protocol p) => SendProtocol p where
  sendFile :: p -> FilePath -> FilePath -> IO ()
  makeRemoteDirectory :: p -> FilePath -> IO ()

  sendTree :: p -> FilePath -> FilePath -> IO ()

class (Protocol p) => ReceiveProtocol p where
  receiveFile :: p -> FilePath -> FilePath -> IO ()
  receiveTree :: p -> FilePath -> FilePath -> IO ()

