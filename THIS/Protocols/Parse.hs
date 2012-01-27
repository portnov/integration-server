module THIS.Protocols.Parse where

import Control.Applicative
import Data.Object
import Data.Object.Yaml

import THIS.Types
import THIS.Yaml
import THIS.Protocols
import THIS.Protocols.LibSSH2
import THIS.Protocols.SSHCommands

parseProtocol :: String -> ConnectionInfo -> IO AnyProtocol
parseProtocol "libssh2" cfg =
    AnyProtocol <$> (connect cfg :: IO LibSSH2)
parseProtocol "ssh-commnands" cfg =
    AnyProtocol <$> (connect cfg :: IO SSHCommands)
parseProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseCommandProtocol :: String -> ConnectionInfo -> IO AnyCommandProtocol
parseCommandProtocol "libssh2" cfg =
    AnyCommandProtocol <$> (connect cfg :: IO LibSSH2)
parseCommandProtocol "ssh-commnands" cfg =
    AnyCommandProtocol <$> (connect cfg :: IO SSHCommands)
parseCommandProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseSendProtocol :: String -> ConnectionInfo -> IO AnySendProtocol
parseSendProtocol "libssh2" cfg =
    AnySendProtocol <$> (connect cfg :: IO LibSSH2)
parseSendProtocol "ssh-commnands" cfg =
    AnySendProtocol <$> (connect cfg :: IO SSHCommands)
parseSendProtocol p _ = fail $ "Unsupported protocol: " ++ p

parseReceiveProtocol :: String -> ConnectionInfo -> IO AnyReceiveProtocol
parseReceiveProtocol "libssh2" cfg =
    AnyReceiveProtocol <$> (connect cfg :: IO LibSSH2)
parseReceiveProtocol "ssh-commnands" cfg =
    AnyReceiveProtocol <$> (connect cfg :: IO SSHCommands)
parseReceiveProtocol p _ = fail $ "Unsupported protocol: " ++ p

