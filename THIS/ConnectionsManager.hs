{-# LANGUAGE ScopedTypeVariables #-}
module THIS.ConnectionsManager where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error
import Control.Concurrent.STM
import Data.Object
import Data.Object.Yaml
import qualified Data.Map as M

import THIS.Types
import THIS.Yaml
import THIS.Protocols.Types
import THIS.Protocols.Parse
import THIS.Protocols

data Protocols = Protocols {
    commandProtocols :: M.Map String AnyCommandProtocol,
    sendProtocols :: M.Map String AnySendProtocol,
    receiveProtocols :: M.Map String AnyReceiveProtocol,
    connectionInfo :: ConnectionInfo,
    commandProtoName :: String,
    sendProtoName :: String,
    receiveProtoName :: String }

type Manager = TVar Protocols

type Managed a = ReaderT Manager IO a

forceEither :: Either YamlError a -> IO a
forceEither (Right x) = return x
forceEither (Left e)  = fail e

manageConnections :: [(String, String)] -> Managed a -> IO a
manageConnections pairs fn = do
  cfg <- forceEither $ loadConnectionInfo pairs
  let generic = lookupDefault "protocol" "libssh2" pairs
      cmdP  = lookupDefault "command-protocol" generic pairs
      sendP = lookupDefault "send-protocol"    cmdP pairs
      recvP = lookupDefault "receive-protocol" cmdP pairs
  manager <- liftIO $ atomically $ newTVar $ Protocols {
                commandProtocols = M.empty,
                sendProtocols = M.empty,
                receiveProtocols = M.empty,
                connectionInfo = cfg,
                commandProtoName = cmdP,
                sendProtoName = sendP,
                receiveProtoName = recvP }
  runReaderT (do
              r <- fn
              freeAllProtocols
              return r) manager

getCommandProtocol :: String -> Managed AnyCommandProtocol
getCommandProtocol host = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  case M.lookup host (commandProtocols prs) of
    Just p -> return p
    Nothing -> do
               let proto = commandProtoName prs
                   ci    = connectionInfo   prs
               new <- liftIO $ parseCommandProtocol proto ci
               let m = M.insert host new (commandProtocols prs)
                   p = prs { commandProtocols = m }
               liftIO $ atomically $ writeTVar var p
               return new

getSendProtocol :: String -> Managed AnySendProtocol
getSendProtocol host = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  case M.lookup host (sendProtocols prs) of
    Just p -> return p
    Nothing -> do
               let proto = sendProtoName prs
                   ci    = connectionInfo   prs
               new <- liftIO $ parseSendProtocol proto ci
               let m = M.insert host new (sendProtocols prs)
                   p = prs { sendProtocols = m }
               liftIO $ atomically $ writeTVar var p
               return new

getReceiveProtocol :: String -> Managed AnyReceiveProtocol
getReceiveProtocol host = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  case M.lookup host (receiveProtocols prs) of
    Just p -> return p
    Nothing -> do
               let proto = receiveProtoName prs
                   ci    = connectionInfo   prs
               new <- liftIO $ parseReceiveProtocol proto ci
               let m = M.insert host new (receiveProtocols prs)
                   p = prs { receiveProtocols = m }
               liftIO $ atomically $ writeTVar var p
               return new

freeAllProtocols :: Managed ()
freeAllProtocols = do
  freeCommandProtocols
  freeSendProtocols
  freeReceiveProtocols

freeCommandProtocols :: Managed ()
freeCommandProtocols = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  forM_ (M.elems $ commandProtocols prs) $ \(AnyCommandProtocol p) ->
      liftIO $ disconnect p

freeSendProtocols :: Managed ()
freeSendProtocols = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  forM_ (M.elems $ sendProtocols prs) $ \(AnySendProtocol p) ->
      liftIO $ disconnect p

freeReceiveProtocols :: Managed ()
freeReceiveProtocols = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  forM_ (M.elems $ receiveProtocols prs) $ \(AnyReceiveProtocol p) ->
      liftIO $ disconnect p





