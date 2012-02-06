
import Control.Monad.Error
import System.Environment

import THIS.Types
import THIS.Config.Global
import THIS.Config.ProjectConfig
import THIS.Database
import THIS.Notify

subject = "Notify: project ${project}, phase ${phase}"

body = "Phase ${phase} of project ${project} has been done with result ${result}"

extVars = [("environment", "work"), ("target", "testing")]

main = do
  [project, phase] <- getArgs
  let result = "ok"
  runErrorT $ do
    gc <- loadGlobalConfig
    chosts <- loadCommonHosts
    (ppath, object, pc) <- loadProjectConfig project extVars chosts
    pid <- runDB (gcDatabase gc) $ checkProject ppath project pc
    let vars = [("project", pcTitle pc),
                ("path",    ppath),
                ("phase",   phase),
                ("result",  result)] ++ extVars
    sendNotifications gc pid pc object vars phase result
  return ()
