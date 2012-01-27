
module THIS.Protocols.All where

import THIS.Protocols
import THIS.Protocols.Parse
import THIS.Protocols.LibSSH2
import THIS.Protocols.SSHCommands

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
