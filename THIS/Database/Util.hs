{-# LANGUAGE TemplateHaskell, QuasiQuotes, EmptyDataDecls, TypeFamilies, FlexibleContexts, GADTs, OverloadedStrings #-}
module THIS.Database.Util where

import Control.Monad
import Control.Monad.Trans
import Database.Persist
import Database.Persist.Postgresql
import qualified Data.ByteString as B
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char

import THIS.Types
import THIS.Util
import THIS.Database.Types
import THIS.Database.Entities

runDB :: (MonadIO m) => DBConfig -> DB a -> m a
runDB dbc db = do
  liftIO $ print dbc
  liftIO $ withPostgresqlConn (toBS $ show dbc) $ runSqlConn $ db

check :: (Show v, PersistEntity v) => v -> DB (Key SqlPersist v)
check v = do
  r <- insertBy v
  case r of
    Left e -> return (entityKey e)
    Right k -> return k

checkGroup :: String -> DB GroupId
checkGroup name = do
  let group = Group (Text.pack name)
  liftIO $ putStrLn $ "checking: " ++ show group
  check group

checkUser :: String -> String -> DB UserId
checkUser group name = do
  gid <- checkGroup group
  let user = User {
               userLogin = name,
               userPassword = "",
               userGroup = gid,
               userEmail = "",
               userFullName = Text.pack name }
  liftIO $ putStrLn $ "checking: " ++ show user
  check user

checkProject :: FilePath -> String -> ProjectConfig -> DB ProjectId
checkProject path slug pc = do
  ownerId <- checkUser "UNKNOWN" (pcOwner pc)
  let project = Project {
                  projectSlug = slug,
                  projectTitle = Text.pack $ pcTitle pc,
                  projectOwner = ownerId,
                  projectFilePath = Text.pack path }
  liftIO $ putStrLn $ "checking: " ++ show project
  check project

