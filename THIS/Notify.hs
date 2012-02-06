
module THIS.Notify where

import Control.Monad
import Control.Monad.Trans
import Control.Failure
import Data.Object
import System.Process
import System.IO
import Text.Printf
import qualified Data.Text as Text
import Database.Persist

import THIS.Types
import THIS.Util
import THIS.Templates.Text
import THIS.Database

composeMail :: String -> String -> String -> String -> String
composeMail from to subj body =
  printf "From: %s\n\
\To: %s\n\
\Subject: %s\n\n\
\%s\n" from to subj body

notify :: GlobalConfig -> String -> String -> String -> StringObject -> Variables -> THIS ()
notify gc address subjtpl bodytpl object vars = do
  let vars' = [("to", address), ("from", gcMailFrom gc)] ++ vars
  sendmail <- liftEitherWith ParsecError $
                 evalTemplate "<sendmail template>" object vars' (gcSendmail gc)
  subject <- liftEitherWith ParsecError $
                 evalTemplate "<subject template>" object vars' subjtpl
  body <- liftEitherWith ParsecError $
                 evalTemplate "<body template>" object vars' bodytpl
  let mail = composeMail (gcMailFrom gc) address subject body
  (stdin, stdout, stderr, pid) <- liftIO $ runInteractiveCommand sendmail
  liftIO $ do
    hPutStrLn stdin mail
    hClose stdin
    waitForProcess pid
  return ()

sendNotifications :: GlobalConfig -> ProjectId -> ProjectConfig -> StringObject -> Variables -> String -> String -> THIS ()
sendNotifications gc pid pc object vars phase result = do
  ts <- runDB (gcDatabase gc) $ do
      rs <- selectList [NotifySetProject <-. [Just pid, Nothing],
                        NotifySetPhase   <-. [Just phase, Nothing],
                        NotifySetResult  <-. [Just result, Nothing] ] []
      liftIO $ print rs
      forM rs $ \set -> do
        us <- selectList [UserGroup ==. notifySetGroup (entityVal set)] []
        let tplId = notifySetTemplate (entityVal set)
        mb <-  get tplId
        case mb of
          Just tpl -> return (map entityVal us, templateSubject tpl, templateBody tpl)
          Nothing -> fail $ "No template with ID " ++ show tplId
  liftIO $ print ts
  forM_ ts $ \(users, subject, body) ->
    forM users $ \user -> do
      let vars' = [("to",   userEmail user),
                   ("from", gcMailFrom gc),
                   ("user", Text.unpack $ userFullName user)] ++ vars
      notify gc (userEmail user) (Text.unpack subject) (Text.unpack body) object vars'

