module THIS.Protocols.Parse where

import Control.Applicative
import Data.Object
import Data.Object.Yaml

import THIS.Types
import THIS.Yaml
import THIS.Protocols.Types
import THIS.Protocols.LibSSH2
import THIS.Protocols.SSHCommands

parseProtocol :: String -> ConnectionInfo -> IO AnyConnection
parseProtocol "libssh2" cfg =
    AnyConnection <$> (connect cfg :: IO LibSSH2)
parseProtocol "ssh-commnands" cfg =
    AnyConnection <$> (connect cfg :: IO SSHCommands)
parseProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseCommandProtocol :: String -> ConnectionInfo -> IO AnyCommandConnection
parseCommandProtocol "libssh2" cfg =
    AnyCommandConnection <$> (connect cfg :: IO LibSSH2)
parseCommandProtocol "ssh-commnands" cfg =
    AnyCommandConnection <$> (connect cfg :: IO SSHCommands)
parseCommandProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseSendProtocol :: String -> ConnectionInfo -> IO AnySendConnection
parseSendProtocol "libssh2" cfg =
    AnySendConnection <$> (connect cfg :: IO LibSSH2)
parseSendProtocol "ssh-commnands" cfg =
    AnySendConnection <$> (connect cfg :: IO SSHCommands)
parseSendProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseReceiveProtocol :: String -> ConnectionInfo -> IO AnyReceiveConnection
parseReceiveProtocol "libssh2" cfg =
    AnyReceiveConnection <$> (connect cfg :: IO LibSSH2)
parseReceiveProtocol "ssh-commnands" cfg =
    AnyReceiveConnection <$> (connect cfg :: IO SSHCommands)
parseReceiveProtocol p _ = fail $ "Unsupported protocol: " ++ p

