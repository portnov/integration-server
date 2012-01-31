{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module THIS.Hypervisor where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Exception as E
import Data.Object
import System.LibVirt as V

import THIS.Types
import THIS.Yaml
import THIS.Templates.XML

runVM :: StringObject -> [(String, String)] -> VMConfig -> IO ()
runVM object vars vm =
  withConnection (vmHypervisor vm)  $ \conn -> do
    mbdom <- do
             x <- E.try $ lookupDomainName conn (vmName vm)
             case x of
               Left (e :: V.Error) -> do
                                      print e
                                      return Nothing
               Right dom -> return (Just dom)
    case mbdom of
      Just dom -> do
        di <- getDomainInfo dom
        case diState di of
          DomainRunning -> return ()
          DomainPaused -> do
            if vmEmpty vm
              then restoreDomain conn (vmSnapshot vm) >> return ()
              else resumeDomain dom >> return ()
          DomainShutoff -> do
            if vmEmpty vm
              then restoreDomain conn (vmSnapshot vm) >> return ()
              else createDomain dom >> return ()
          st -> fail $ "Don't know what to do with virtual domain " ++ vmName vm ++ " in state " ++ show st
      Nothing -> do
        xml <- forceTHIS XMLTemplateError $ evalXMLFile object vars (vmTemplatePath vm)
        createDomainXML conn xml []
        return ()
