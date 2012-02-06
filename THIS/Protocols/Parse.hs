module THIS.Protocols.Parse where

import Control.Applicative

import THIS.Protocols.Types
import THIS.Protocols.Local
import THIS.Protocols.LibSSH2
import THIS.Protocols.SSHCommands

parseProtocol :: String -> ConnectionInfo -> IO AnyConnection
parseProtocol "local" cfg =
    AnyConnection <$> (connect cfg :: IO Local)
parseProtocol "libssh2" cfg =
    AnyConnection <$> (connect cfg :: IO LibSSH2)
parseProtocol "ssh-commnands" cfg =
    AnyConnection <$> (connect cfg :: IO SSHCommands)
parseProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseCommandProtocol :: String -> ConnectionInfo -> IO AnyCommandConnection
parseCommandProtocol "local" cfg =
    AnyCommandConnection <$> (connect cfg :: IO Local)
parseCommandProtocol "libssh2" cfg =
    AnyCommandConnection <$> (connect cfg :: IO LibSSH2)
parseCommandProtocol "ssh-commnands" cfg =
    AnyCommandConnection <$> (connect cfg :: IO SSHCommands)
parseCommandProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseSendProtocol :: String -> ConnectionInfo -> IO AnyFilesConnection
parseSendProtocol "local" cfg =
    AnyFilesConnection <$> (connect cfg :: IO Local)
parseSendProtocol "libssh2" cfg =
    AnyFilesConnection <$> (connect cfg :: IO LibSSH2)
parseSendProtocol "ssh-commnands" cfg =
    AnyFilesConnection <$> (connect cfg :: IO SSHCommands)
parseSendProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseReceiveProtocol :: String -> ConnectionInfo -> IO AnyFilesConnection
parseReceiveProtocol "local" cfg =
    AnyFilesConnection <$> (connect cfg :: IO Local)
parseReceiveProtocol "libssh2" cfg =
    AnyFilesConnection <$> (connect cfg :: IO LibSSH2)
parseReceiveProtocol "ssh-commnands" cfg =
    AnyFilesConnection <$> (connect cfg :: IO SSHCommands)
parseReceiveProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseTransferProtocol :: String -> ConnectionInfo -> ConnectionInfo -> IO TransferConnections
parseTransferProtocol "local" cfg1 cfg2 =
    TransferConnections <$> (connect cfg1 :: IO Local) <*> (connect cfg2 :: IO Local)
parseTransferProtocol "libssh2" cfg1 cfg2 =
    TransferConnections <$> (connect cfg1 :: IO LibSSH2) <*> (connect cfg2 :: IO LibSSH2)
parseTransferProtocol "ssh-commands" cfg1 cfg2 =
    TransferConnections <$> (connect cfg1 :: IO SSHCommands) <*> (connect cfg2 :: IO SSHCommands)

