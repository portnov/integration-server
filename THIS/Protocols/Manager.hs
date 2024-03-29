{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
-- | Hosts connections manager
module THIS.Protocols.Manager
  (manageConnections,
   getCommandConnection,
   getSendConnection,
   getReceiveConnection,
   getTransferConnections
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State as St
import Control.Failure
import Data.Typeable
import Data.Maybe
import qualified Data.Map as M

import THIS.Types
import THIS.Protocols.Types
import THIS.Protocols.Parse

lookupDefault :: String -> String -> Variables -> String
lookupDefault key def pairs = fromMaybe def $ lookup key pairs

lookupForce :: String -> Variables -> Either ErrorMessage String
lookupForce key pairs =
  case lookup key pairs of
    Nothing -> failure $ "Key not found: " ++ key
    Just v  -> return v

manageConnections :: (MonadIO m, Failure ErrorMessage m) => [HostConfig] -> Managed m a -> m a
manageConnections hosts fn = do
  let manager = Connections {
                  commandConnections  = M.empty,
                  sendConnections     = M.empty,
                  receiveConnections  = M.empty,
                  transferConnections = M.empty }
  evalStateT (do
              r <- fn
              freeAllConnections
              return r) manager

pairFileConnections :: AnyFilesConnection -> AnyFilesConnection -> Maybe TransferConnections
pairFileConnections (AnyFilesConnection p1) (AnyFilesConnection p2) = do
  p2' <- cast p2
  return $ TransferConnections p1 p2'

getCommandConnection :: (MonadIO m, Failure ErrorMessage m) => HostConfig -> Managed m AnyCommandConnection
getCommandConnection host = do
  prs <- St.get
  case M.lookup (hcHostname host) (commandConnections prs) of
    Just p -> return p
    Nothing -> do
               let proto = hcCommandsProtocol host
                   ci    = hcConnectionInfo host
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
                   ci    = hcConnectionInfo host
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
                   ci    = hcConnectionInfo host
               new <- liftIO $ parseReceiveProtocol proto ci
               let m = M.insert (hcHostname host) new (receiveConnections prs)
               St.put $ prs { receiveConnections = m }
               return new

getTransferConnections :: (MonadIO m, Failure ErrorMessage m) => HostConfig -> HostConfig -> Managed m (Maybe TransferConnections)
getTransferConnections src dst = do
  if hcReceiveProtocol src == hcSendProtocol dst
    then do
         prs <- St.get
         let pair = (hcHostname src, hcHostname dst)
         case M.lookup pair (transferConnections prs) of
           Just tc -> return (Just tc)
           Nothing -> do
               let proto = hcReceiveProtocol src
               srcp <- getReceiveConnection src
               dstp <- getSendConnection    dst
               case pairFileConnections srcp dstp of
                 Nothing -> do
                   liftIO $ putStrLn $ "Unexpected: cannot pair connections to " ++ hcHostname src ++ " and " ++ hcHostname dst
                   return Nothing
                 Just new -> do
                   let m = M.insert pair new (transferConnections prs)
                   St.put $ prs { transferConnections = m }
                   return (Just new)
    else return Nothing

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

