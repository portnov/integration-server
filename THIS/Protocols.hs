module THIS.Protocols where

import Control.Applicative
import Control.Failure
import Data.Object
import Data.Object.Yaml
import Data.Maybe

import THIS.Types
import THIS.Yaml
import THIS.Protocols.Types
import THIS.Protocols.Parse
import THIS.Protocols.LibSSH2
import THIS.Protocols.SSHCommands

lookupDefault :: String -> String -> [(String, String)] -> String
lookupDefault key def pairs = fromMaybe def $ lookup key pairs

lookupForce :: String -> [(String, String)] -> Either ErrorMessage String
lookupForce key pairs =
  case lookup key pairs of
    Nothing -> failure $ "Key not found: " ++ key
    Just v  -> return v

loadConnectionInfo :: [(String, String)] -> Either ErrorMessage ConnectionInfo
loadConnectionInfo pairs = ConnectionInfo
    <$> lookupForce "host" pairs
    <*> (readInt $ lookupDefault "port" "22" pairs)
    <*> (Right $ lookupDefault "login" "this" pairs)
    <*> (Right $ lookupDefault "known-hosts" kh pairs)
    <*> (Right $ lookupDefault "public-key" pub pairs)
    <*> (Right $ lookupDefault "private-key" priv pairs)
  where
    kh   = "/etc/this/ssh/known_hosts"
    pub  = "/etc/this/ssh/id_rsa.pub"
    priv = "/etc/this/ssh/id_rsa"

initializeProtocols :: IO ()
initializeProtocols = do
  initializeProtocol (LibSSH2 undefined undefined)
  initializeProtocol (SSHCommands undefined)

deinitializeProtocols :: IO ()
deinitializeProtocols = do
  deinitializeProtocol (LibSSH2 undefined undefined)
  deinitializeProtocol (SSHCommands undefined)

disconnectA :: AnyProtocol -> IO ()
disconnectA (AnyProtocol p) = disconnect p

runCommandsA :: AnyCommandProtocol -> [String] -> IO (Int, [String])
runCommandsA (AnyCommandProtocol p) commands =
  runCommands p commands

chdirA :: AnyCommandProtocol -> FilePath -> IO ()
chdirA (AnyCommandProtocol p) dir = changeWorkingDirectory p dir

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

