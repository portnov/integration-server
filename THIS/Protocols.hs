module THIS.Protocols where

import Control.Applicative
import Data.Object
import Data.Object.Yaml

import THIS.Types
import THIS.Yaml
import THIS.Protocols.Types
import THIS.Protocols.Parse
import THIS.Protocols.LibSSH2
import THIS.Protocols.SSHCommands

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

initializeProtocols :: IO ()
initializeProtocols = do
  initializeProtocol (LibSSH2 undefined)
  initializeProtocol (SSHCommands undefined)

deinitializeProtocols :: IO ()
deinitializeProtocols = do
  deinitializeProtocol (LibSSH2 undefined)
  deinitializeProtocol (SSHCommands undefined)

runCommandA :: AnyCommandProtocol -> String -> IO (Int, String)
runCommandA (AnyCommandProtocol p) command =
  runCommand p command

sendFileA :: AnySendProtocol -> FilePath -> FilePath -> IO ()
sendFileA (AnySendProtocol p) local remote =
  sendFile p local remote

makeRemoteDirectoryA :: AnySendProtocol -> FilePath -> IO ()
makeRemoteDirectoryA (AnySendProtocol p) path =
  makeRemoteDirectory p path

sendTreeA :: AnySendProtocol -> FilePath -> FilePath -> IO ()
sendTreeA (AnySendProtocol p) local remote =
  sendTree p local remote

receiveFileA :: AnyReceiveProtocol -> FilePath -> FilePath -> IO ()
receiveFileA (AnyReceiveProtocol p) remote local =
  receiveFile p remote local

receiveTreeA :: AnyReceiveProtocol -> FilePath -> FilePath -> IO ()
receiveTreeA (AnyReceiveProtocol p) remote local =
  receiveTree p remote local

