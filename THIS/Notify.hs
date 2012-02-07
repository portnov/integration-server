--  | Send notifications by e-mail.
module THIS.Notify
  (sendNotifications
  ) where

import Control.Monad
import Control.Monad.Trans
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

-- | Compose text to pass to sendmail command.
composeMail :: String  -- ^ From:
            -> String  -- ^ To:
            -> String  -- ^ Subject:
            -> String  -- ^ Message body
            -> String
composeMail from to subj body =
  printf "From: %s\n\
\To: %s\n\
\Subject: %s\n\n\
\%s\n" from to subj body

-- | Send one notification.
notify :: GlobalConfig
       -> String       -- ^ E-mail to send notification to
       -> String       -- ^ Message subject template
       -> String       -- ^ Message body template
       -> StringObject
       -> Variables
       -> THIS ()
notify gc address subjtpl bodytpl object vars = do
  let vars' = [("to", address), ("from", gcMailFrom gc)] ++ vars
  sendmail <- liftEither $
                 evalTemplate "<sendmail template>" object vars' (gcSendmail gc)
  subject <- liftEither $
                 evalTemplate "<subject template>" object vars' subjtpl
  body <- liftEither $
                 evalTemplate "<body template>" object vars' bodytpl
  let mail = composeMail (gcMailFrom gc) address subject body
  liftIO $ putStrLn $ "> " ++ sendmail
  liftIO $ putStrLn $ mail
  (stdin, stdout, stderr, pid) <- liftIO $ runInteractiveCommand sendmail
  liftIO $ do
    hPutStrLn stdin mail
    hClose stdin
    waitForProcess pid
  return ()

-- | Send notifications to all interested people
sendNotifications :: GlobalConfig
                  -> ProjectId
                  -> ProjectConfig
                  -> StringObject
                  -> Variables
                  -> String        -- ^ Phase name
                  -> String        -- ^ Phase result
                  -> THIS ()
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

