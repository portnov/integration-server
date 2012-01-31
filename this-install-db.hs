{-# LANGUAGE OverloadedStrings #-}
import Database.Persist.Postgresql
import Data.Text as Text

import THIS.Database.Entities

main :: IO ()
main = do
  withPostgresqlConn "dbname=this" $ runSqlConn $ do
    runMigration migrateAll
