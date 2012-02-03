module THIS.Protocols where

import Control.Applicative
import Control.Failure
import Data.Object
import Data.Object.Yaml
import Data.Maybe
import Data.Conduit

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

