{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
-- | Local protocol: executes commands and copies files
-- on localhost. Used by default for `this' host.
module THIS.Protocols.Local
  (Local (..),
   copyDir
  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Control.Exception
import Data.Monoid
import Data.Conduit
import Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Generics
import System.Process
import System.IO
import System.FilePath
import System.Directory

import THIS.Types
import THIS.Util
import THIS.Yaml
import THIS.Protocols.Types

data Local = Local
  deriving (Data, Typeable)

instance Protocol Local where
  initializeProtocol _ = return ()
  deinitializeProtocol _ = return ()

  connect _ = return Local
  disconnect _ = return ()

instance CommandProtocol Local where
  data RCHandle Local = RCHL (TMVar ProcessHandle)

  runCommands Local commands = do
      var <- newEmptyTMVarIO
      return (RCHL var, source var)
    where
      source var =
        let bs = replicate (length commands - 1) False ++ [True]
        in  mconcat [sourceIOHandle (run b var cmd)
                     $= CB.lines
                     $= CL.map fromBS
                     | (b, cmd) <- zip bs commands]

      run b var command = do
        (stdin,stdout,stderr, pid) <- runInteractiveCommand command
        hClose stdin
        when b $
          atomically $ putTMVar var pid
        return stdout

  getExitStatus (RCHL var) = do
    pid <- atomically $ takeTMVar var
    rc <- waitForProcess pid
    return (rc2int rc)

  changeWorkingDirectory _ dir = setCurrentDirectory dir

instance FilesProtocol Local where
  sendFile _  from to =
    copyFile from to

  makeRemoteDirectory _ path = createDirectory path

  sendTree _ from to = copyDir from to

  receiveFile _ from to =
    copyFile from to

  receiveTree _ from to = copyDir from to

  transferFile _ from _ to = copyFile from to

  transferTree _ from _ to = copyDir from to

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  whenM (not <$> doesDirectoryExist src) $
    throw (userError "source does not exist")
  whenM (doesFileOrDirectoryExist dst) $
    throw (userError "destination already exists")

  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath

  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r

