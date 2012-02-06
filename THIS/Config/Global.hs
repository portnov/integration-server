{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Global config (config.yaml) parser
module THIS.Config.Global
  (loadGlobalConfig
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Failure
import Data.Object
import Data.Object.Yaml
import System.Environment
import System.FilePath
import System.Directory

import THIS.Types
import THIS.Util
import THIS.Yaml

-- | Load global configuration from config.yaml
loadGlobalConfig :: THIS GlobalConfig
loadGlobalConfig = do
  home <- liftIO $ getEnv "HOME"
  let homePath = home </> ".config" </> "this" </> "config.yaml"
      etcPath  = "/etc" </> "this" </> "config.yaml"
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
    Left err -> failure err
    Right object -> liftEither $ convertConfig object

convertConfig :: StringObject -> Either ErrorMessage GlobalConfig
convertConfig object =
  GlobalConfig
    <$> (convertDBC =<< get "database" object)
    <*> getOptional "sendmail" "sendmail -t" object
    <*> getOptional "mail-from" "this@localhost" object

convertDBC :: StringObject -> Either ErrorMessage DBConfig
convertDBC object = 
  DBConfig
    <$> getOptional "host" "" object
    <*> getOptional "port" 5432 object
    <*> getOptional "database" "this" object
    <*> getOptional "user" "this" object
    <*> getOptional "password" "" object

