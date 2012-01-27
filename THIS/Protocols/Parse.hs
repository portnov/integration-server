
module THIS.Protocols.Parse where

import THIS.Protocols
import THIS.Protocols.LibSSH2
import THIS.Protocols.SSHCommands

parseProtocol :: String -> Either String AnyProtocol
parseProtocol "libssh2"       = Right (AnyProtocol (LibSSH2 undefined))
parseProtocol "ssh-commnands" = Right (AnyProtocol (SSHCommands undefined))
parseProtocol p = Left $ "Unsupported protocol: " ++ p

parseCommandProtocol :: String -> Either String AnyCommandProtocol
parseCommandProtocol "libssh2"       = Right (AnyCommandProtocol (LibSSH2 undefined))
parseCommandProtocol "ssh-commnands" = Right (AnyCommandProtocol (SSHCommands undefined))
parseCommandProtocol p = Left $ "Unsupported protocol: " ++ p

parseSendProtocol :: String -> Either String AnySendProtocol
parseSendProtocol "libssh2"       = Right (AnySendProtocol (LibSSH2 undefined))
parseSendProtocol "ssh-commnands" = Right (AnySendProtocol (SSHCommands undefined))
parseSendProtocol p = Left $ "Unsupported protocol: " ++ p

parseReceiveProtocol :: String -> Either String AnyReceiveProtocol
parseReceiveProtocol "libssh2"       = Right (AnyReceiveProtocol (LibSSH2 undefined))
parseReceiveProtocol "ssh-commnands" = Right (AnyReceiveProtocol (SSHCommands undefined))
parseReceiveProtocol p = Left $ "Unsupported protocol: " ++ p

