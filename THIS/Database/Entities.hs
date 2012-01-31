{-# LANGUAGE TemplateHaskell, QuasiQuotes, EmptyDataDecls, TypeFamilies, FlexibleContexts, GADTs, OverloadedStrings #-}
module THIS.Database.Entities where

import Data.Time
import Database.Persist
import Database.Persist.TH
import Data.Text

import THIS.Types

data Role = Admin | View
  deriving (Eq, Show, Read)

derivePersistField "Role"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Group
  title Text

User
  login String
  password String
  group GroupId
  email String
  fullName Text

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
  returnCode Int
  result String

OutputGroupRecord
  actionRecord ActionRecordId
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
