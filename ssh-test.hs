
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception as E
import System.Process
import System.IO

readData :: Handle -> IO String
readData h = do
  b <- hIsReadable h
  if b
    then do
         s <- hGetLine h
         putStrLn $ "Read: " ++ s
         t <- readData h
         return $ s ++ "\n" ++ t
    else return ""

communicate inh outh input = do
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    forkIO $ E.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do
        hPutStrLn inh input
        hFlush inh

    -- wait on the output
    takeMVar outMVar

main = do
  (stdin, stdout, stderr, ssh) <- runInteractiveCommand "ssh -C home"
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  result <- communicate stdin stdout "ls"
  print result
  hClose stdin
  waitForProcess ssh
  return ()
