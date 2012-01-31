{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module THIS.Config.Global where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import Control.Failure
import Data.Object
import Data.Object.Yaml
import System.Environment
import System.FilePath
import System.Directory

import THIS.Types
import THIS.Yaml

loadGlobalConfig :: YamlM GlobalConfig
loadGlobalConfig = do
  home <- liftIO $ getEnv "HOME"
  let homePath = home </> ".config" </> "this" </> "config.yaml"
      etcPath  = "/etc" </> "thin" </> "config.yaml"
  he <- liftIO $ doesFileExist homePath
  path <- if he
            then return homePath
            else do
                 ee <- liftIO $ doesFileExist etcPath
                 if ee
                   then return etcPath
                   else failure $ "Global config file not found neither in /etc nor ~: config.yaml"
  x <- liftIO $ (decodeFile path :: IO (Either ParseException StringObject))
  case x of
    Left err -> failure (show err)
    Right object -> ErrorT $ return $ convertConfig object

convertConfig :: StringObject -> Either YamlError GlobalConfig
convertConfig object =
  GlobalConfig
    <$> (convertDBC =<< get "database" object)
    <*> getOptional "sendmail" "sendmail -t" object

convertDBC :: StringObject -> Either YamlError DBConfig
convertDBC object = 
  DBConfig
    <$> getOptional "host" "" object
    <*> getOptional "port" 5432 object
    <*> getOptional "database" "thin" object
    <*> getOptional "user" "thin" object
    <*> getOptional "password" "" object
