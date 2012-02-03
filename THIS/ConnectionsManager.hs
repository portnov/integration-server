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
import Control.Monad.State as St
import Control.Monad.Error
import Control.Failure
import Data.Object
import Data.Object.Yaml
import qualified Data.Map as M
import qualified Text.Parsec as P

import THIS.Types
import THIS.Util
import THIS.Yaml
import THIS.Protocols.Types
import THIS.Protocols.Parse
import THIS.Protocols

data Connections = Connections {
    commandConnections :: M.Map String AnyCommandConnection,
    sendConnections    :: M.Map String AnyFilesConnection,
    receiveConnections :: M.Map String AnyFilesConnection }

type Managed m a = StateT Connections m a

type MTHIS a = StateT Connections (ErrorT ErrorMessage IO) a

instance (Monad m, Failure e m) => Failure e (StateT Connections m) where
  failure e = lift (failure e)

manageConnections :: (MonadIO m, Failure ErrorMessage m) => [HostConfig] -> Managed m a -> m a
manageConnections hosts fn = do
  let manager = Connections {
                commandConnections = M.empty,
                sendConnections = M.empty,
                receiveConnections = M.empty }
  evalStateT (do
              r <- fn
              freeAllConnections
              return r) manager

getCommandConnection :: (MonadIO m, Failure ErrorMessage m) => HostConfig -> Managed m AnyCommandConnection
getCommandConnection host = do
  prs <- St.get
  case M.lookup (hcHostname host) (commandConnections prs) of
    Just p -> return p
    Nothing -> do
               let proto = hcCommandsProtocol host
               ci <- forceEither $ loadConnectionInfo (hcParams host)
               new <- liftIO $ parseCommandProtocol proto ci
               let m = M.insert (hcHostname host) new (commandConnections prs)
               St.put $ prs { commandConnections = m }
               return new

getSendConnection :: (MonadIO m, Failure ErrorMessage m) => HostConfig -> Managed m AnyFilesConnection
getSendConnection host = do
  prs <- St.get
  case M.lookup (hcHostname host) (sendConnections prs) of
    Just p -> return p
    Nothing -> do
               let proto = hcSendProtocol host
               ci <- forceEither $ loadConnectionInfo (hcParams host)
               new <- liftIO $ parseSendProtocol proto ci
               let m = M.insert (hcHostname host) new (sendConnections prs)
               St.put $ prs { sendConnections = m }
               return new

getReceiveConnection :: (MonadIO m, Failure ErrorMessage m) => HostConfig -> Managed m AnyFilesConnection
getReceiveConnection host = do
  prs <- St.get
  case M.lookup (hcHostname host) (receiveConnections prs) of
    Just p -> return p
    Nothing -> do
               let proto = hcReceiveProtocol host
               ci <- forceEither $ loadConnectionInfo (hcParams host)
               new <- liftIO $ parseReceiveProtocol proto ci
               let m = M.insert (hcHostname host) new (receiveConnections prs)
               St.put $ prs { receiveConnections = m }
               return new

freeAllConnections :: (MonadIO m) => Managed m ()
freeAllConnections = do
  freeCommandConnections
  freeSendConnections
  freeReceiveConnections

freeCommandConnections :: (MonadIO m) => Managed m ()
freeCommandConnections = do
  prs <- St.get
  forM_ (M.elems $ commandConnections prs) $ \(AnyCommandConnection p) ->
      liftIO $ disconnect p

freeSendConnections :: (MonadIO m) => Managed m ()
freeSendConnections = do
  prs <- St.get
  forM_ (M.elems $ sendConnections prs) $ \(AnyFilesConnection p) ->
      liftIO $ disconnect p

freeReceiveConnections :: (MonadIO m) => Managed m ()
freeReceiveConnections = do
  prs <- St.get
  forM_ (M.elems $ receiveConnections prs) $ \(AnyFilesConnection p) ->
      liftIO $ disconnect p

