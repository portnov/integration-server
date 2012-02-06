{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
-- | Virtual machines control
module THIS.Hypervisor
  (runVM,
   shutdownVM
  ) where

import Control.Monad
import Control.Exception as E
import Control.Concurrent
import Data.Object
import Data.Time
import System.LibVirt as V

import THIS.Types
import THIS.Yaml
import THIS.Templates.XML

-- | Run VM if needed
runVM :: StringObject -> Variables -> VMConfig -> IO ()
runVM object vars vm =
  withConnection (vmHypervisor vm)  $ \conn -> do
    putStrLn "connect ok"
    mbdom <- do
             x <- E.try $ lookupDomainName conn (vmName vm)
             case x of
               Left (e :: V.Error) -> do
                                      putStrLn "lookupDomainName:"
                                      print e
                                      return Nothing
               Right dom -> return (Just dom)
    case mbdom of
      Just dom -> do
        di <- getDomainInfo dom
        putStrLn $ "domain got ok: " ++ show di
        case diState di of
          DomainRunning -> return ()
          DomainPaused -> do
            if vmEmpty vm
              then restoreDomain conn (vmSnapshot vm) >> return ()
              else resumeDomain dom >> return ()
          DomainShutoff -> do
            if vmEmpty vm
              then restoreDomain conn (vmSnapshot vm) >> return ()
              else do
                   createDomain dom
                   waitVMStartup vm
                   return ()
          st -> fail $ "Don't know what to do with virtual domain " ++ vmName vm ++ " in state " ++ show st
      Nothing -> do
        xml <- forceTHIS XMLTemplateError $ evalXMLFile object vars (vmTemplatePath vm)
        createDomainXML conn xml []
        return ()

-- | Wait for VM to start up
waitVMStartup :: VMConfig -> IO ()
waitVMStartup vm = do
  now <- getCurrentTime
  putStrLn $ "Waiting for VM to start up: " ++ show (vmStartupTime vm) ++ "s"
  threadDelay (vmStartupTime vm * 1000 * 1000)
  now' <- getCurrentTime
  putStrLn $ show now ++ " - " ++ show now'

-- | Shutdown VM
shutdownVM :: VMConfig -> IO ()
shutdownVM vm =
  withConnection (vmHypervisor vm) $ \conn -> do
    mbdom <- do
             x <- E.try $ lookupDomainName conn (vmName vm)
             case x of
               Left (e :: V.Error) -> do
                                      print e
                                      return Nothing
               Right dom -> return (Just dom)
    case mbdom of
      Nothing -> fail $ "No such domain on hypervisor: " ++ vmName vm
      Just dom -> do
        shutdownDomain dom
        return ()

