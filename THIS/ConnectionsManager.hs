{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
-- | Hosts connections manager
module THIS.ConnectionsManager
  (Managed,
   MTHIS,
   manageConnections,
   getCommandConnection,
   getSendConnection,
   getReceiveConnection
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error
import Control.Failure
import Control.Concurrent.STM
import Data.Object
import Data.Object.Yaml
import qualified Data.Map as M
import qualified Text.Parsec as P

import THIS.Types
import THIS.Yaml
import THIS.Protocols.Types
import THIS.Protocols.Parse
import THIS.Protocols

data Connections = Connections {
    commandConnections :: M.Map String AnyCommandConnection,
    sendConnections :: M.Map String AnyFilesConnection,
    receiveConnections :: M.Map String AnyFilesConnection,
    connectionInfo :: ConnectionInfo,
    commandProtoName :: String,
    sendProtoName :: String,
    receiveProtoName :: String }

type Manager = TVar Connections

type Managed m a = ReaderT Manager m a

type MTHIS a = ReaderT Manager (ErrorT ErrorMessage IO) a

instance (Monad m, Failure P.ParseError m) => Failure P.ParseError (ReaderT Manager m) where
  failure e = lift (failure e)

forceEither :: (Monad m, Failure ErrorMessage m) => Either ErrorMessage a -> m a
forceEither (Right x) = return x
forceEither (Left e)  = failure e

manageConnections :: (MonadIO m, Failure ErrorMessage m) => [(String, String)] -> Managed m a -> m a
manageConnections pairs fn = do
  cfg <- forceEither $ loadConnectionInfo pairs
  let generic = lookupDefault "protocol" "libssh2" pairs
      cmdP  = lookupDefault "command-protocol" generic pairs
      sendP = lookupDefault "send-protocol"    cmdP pairs
      recvP = lookupDefault "receive-protocol" cmdP pairs
  manager <- liftIO $ atomically $ newTVar $ Connections {
                commandConnections = M.empty,
                sendConnections = M.empty,
                receiveConnections = M.empty,
                connectionInfo = cfg,
                commandProtoName = cmdP,
                sendProtoName = sendP,
                receiveProtoName = recvP }
  runReaderT (do
              r <- fn
              freeAllConnections
              return r) manager

getCommandConnection :: (MonadIO m) => String -> Managed m AnyCommandConnection
getCommandConnection host = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  case M.lookup host (commandConnections prs) of
    Just p -> return p
    Nothing -> do
               let proto = commandProtoName prs
                   ci    = connectionInfo   prs
               new <- liftIO $ parseCommandProtocol proto ci
               let m = M.insert host new (commandConnections prs)
                   p = prs { commandConnections = m }
               liftIO $ atomically $ writeTVar var p
               return new

getSendConnection :: (MonadIO m) => String -> Managed m AnyFilesConnection
getSendConnection host = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  case M.lookup host (sendConnections prs) of
    Just p -> return p
    Nothing -> do
               let proto = sendProtoName prs
                   ci    = connectionInfo   prs
               new <- liftIO $ parseSendProtocol proto ci
               let m = M.insert host new (sendConnections prs)
                   p = prs { sendConnections = m }
               liftIO $ atomically $ writeTVar var p
               return new

getReceiveConnection :: (MonadIO m) => String -> Managed m AnyFilesConnection
getReceiveConnection host = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  case M.lookup host (receiveConnections prs) of
    Just p -> return p
    Nothing -> do
               let proto = receiveProtoName prs
                   ci    = connectionInfo   prs
               new <- liftIO $ parseReceiveProtocol proto ci
               let m = M.insert host new (receiveConnections prs)
                   p = prs { receiveConnections = m }
               liftIO $ atomically $ writeTVar var p
               return new

freeAllConnections :: (MonadIO m) => Managed m ()
freeAllConnections = do
  freeCommandConnections
  freeSendConnections
  freeReceiveConnections

freeCommandConnections :: (MonadIO m) => Managed m ()
freeCommandConnections = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  forM_ (M.elems $ commandConnections prs) $ \(AnyCommandConnection p) ->
      liftIO $ disconnect p

freeSendConnections :: (MonadIO m) => Managed m ()
freeSendConnections = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  forM_ (M.elems $ sendConnections prs) $ \(AnyFilesConnection p) ->
      liftIO $ disconnect p

freeReceiveConnections :: (MonadIO m) => Managed m ()
freeReceiveConnections = do
  var <- ask
  prs <- liftIO $ atomically $ readTVar var
  forM_ (M.elems $ receiveConnections prs) $ \(AnyFilesConnection p) ->
      liftIO $ disconnect p

