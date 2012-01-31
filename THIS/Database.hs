{-# LANGUAGE RecordWildCards #-}
module THIS.Database
  (module THIS.Database.Types,
   module THIS.Database.Entities,
   module THIS.Database.Util,
   startAction, finishAction,
   logOutput
  ) where

import Control.Monad
import Control.Monad.Trans
import Database.Persist
import Database.Persist.Postgresql
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char
import Data.Time

import THIS.Types
import THIS.Parse
import THIS.Database.Types
import THIS.Database.Entities
import THIS.Database.Util

startAction :: ProjectId -> String -> String -> DB ActionRecordId
startAction pid phase action = do
  now <- liftIO getCurrentTime
  let record = ActionRecord {
                 actionRecordProject = pid,
                 actionRecordPhase   = phase,
                 actionRecordAction  = action,
                 actionRecordStart   = now,
                 actionRecordEnd     = Nothing,
                 actionRecordReturnCode = Nothing,
                 actionRecordResult     = "running" }
  insert record

logOutput :: ActionRecordId -> ParserResult -> DB OutputGroupRecordId
logOutput arid (ParserResult {..}) = do
  let groupRecord = OutputGroupRecord {
                      outputGroupRecordActionRecord = arid,
                      outputGroupRecordGroupName    = prGroupName,
                      outputGroupRecordOtherLines   = Text.pack (unlines prOtherLines) }
  grid <- insert groupRecord
  forM_ prParams $ \(name, value) -> do
      let param = OutputGroupParam {
                    outputGroupParamGroup    = grid,
                    outputGroupParamVariable = name,
                    outputGroupParamValue    = value }
      insert param
  return grid

finishAction :: ActionRecordId -> Int -> String -> DB ()
finishAction arid rc result = do
   now <- liftIO getCurrentTime
   update arid [ActionRecordEnd        =. Just now,
                ActionRecordReturnCode =. Just rc,
                ActionRecordResult     =. result ]

