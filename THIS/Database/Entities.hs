{-# LANGUAGE TemplateHaskell, QuasiQuotes, EmptyDataDecls, TypeFamilies, FlexibleContexts, GADTs, OverloadedStrings #-}
-- | Databasse entities
module THIS.Database.Entities where

import Data.Time
import Database.Persist
import Database.Persist.TH
import Data.Text

import THIS.Types
import THIS.Database.Types

derivePersistField "Role"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Group
  title Text
  UniqueTitle title

User
  login String
  password String
  group GroupId
  email String
  fullName Text
  UniqueLogin login

Project
  slug String
  title Text
  owner UserId
  filePath Text
  UniqueSlug slug

ActionRecord
  project ProjectId
  phase String
  action String
  start UTCTime
  end UTCTime Maybe
  returnCode Int Maybe
  result String

OutputGroupRecord
  actionRecord ActionRecordId
  time UTCTime Maybe
  groupName String
  otherLines Text

OutputGroupParam
  group OutputGroupRecordId
  variable String
  value String

Template
  title Text
  subject Text
  body Text

NotifySet
  project ProjectId Maybe
  group GroupId
  phase String Maybe
  result String Maybe
  template TemplateId

RightsSet
  project ProjectId
  group GroupId
  role Role
|]

