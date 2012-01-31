{-# LANGUAGE TemplateHaskell, QuasiQuotes, EmptyDataDecls, TypeFamilies, FlexibleContexts, GADTs, OverloadedStrings #-}
module THIS.Database.Util where

import Database.Persist
import qualified Data.Text as Text
import Data.Text (Text)

import THIS.Types
import THIS.Database.Types
import THIS.Database.Entities

check :: (PersistEntity v, PersistStore b m, PersistUnique b m) => v -> b m (Key b v)
check v = do
  r <- insertBy v
  case r of
    Left e -> return (entityKey e)
    Right k -> return k

checkGroup :: String -> DB GroupId
checkGroup name = do
  let group = Group {
                groupTitle = Text.pack name }
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
  check user

checkProject :: FilePath -> String -> ProjectConfig -> DB ProjectId
checkProject path slug pc = do
  ownerId <- checkUser "UNKNOWN" (pcOwner pc)
  let project = Project {
                  projectSlug = slug,
                  projectTitle = Text.pack $ pcTitle pc,
                  projectOwner = ownerId,
                  projectFilePath = Text.pack path }
  check project

