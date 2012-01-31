
module THIS.Database.Types where

import Database.Persist.GenericSql

type DB a = SqlPersist IO a

data Role = Admin | View
  deriving (Eq, Show, Read)

