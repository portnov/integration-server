
module THIS.Protocols where

class Protocol p where
  connect :: String -> Int -> IO p
  disconnect :: p -> IO ()

class (Protocol p) => CommandProtocol p where
  runCommand :: p -> String -> IO (Int, String)

class (Protocol p) => SendProtocol p where
  sendFile :: p -> FilePath -> FilePath -> IO ()

class (Protocol p) => ReceiveProtocol p where
  receiveFile :: p -> FilePath -> FilePath -> IO ()

