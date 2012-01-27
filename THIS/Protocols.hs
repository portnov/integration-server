{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}
module THIS.Protocols where

import Control.Applicative
import Data.Object
import Data.Object.Yaml

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

loadConnectionInfo :: StringObject -> Either YamlError ConnectionInfo
loadConnectionInfo object = ConnectionInfo
    <$> get "host" object
    <*> getOptional "port" 22 object
    <*> getOptional "login" "this" object
    <*> getOptional "known-hosts" kh object
    <*> getOptional "public-key" pub object
    <*> getOptional "private-key" priv object
  where
    kh   = "/etc/this/ssh/known_hosts"
    pub  = "/etc/this/ssh/id_rsa.pub"
    priv = "/etc/this/ssh/id_rsa"

class Protocol p where
  initializeProtocol :: p -> IO ()
  deinitializeProtocol :: p -> IO ()

  connect :: ConnectionInfo -> IO p
  disconnect :: p -> IO ()

data AnyProtocol = forall p. Protocol p => AnyProtocol p

class (Protocol p) => CommandProtocol p where
  runCommand :: p -> String -> IO (Int, String)

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

