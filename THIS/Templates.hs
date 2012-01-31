
module THIS.Templates where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Failure
import System.FilePath
import System.Directory
import System.Environment
import System.Random

import THIS.Types
import THIS.Yaml

tempFile :: IO FilePath
tempFile = do
  r <- replicateM 5 $ randomRIO ('0', '9')
  return $ "/tmp/this" ++ r

readTemplate :: String -> THIS (FilePath, String)
readTemplate name = do
  home <- liftIO $ getEnv "HOME"
  let homePath = home </> ".config" </> "this" </> "templates" </> name
      etcPath  = "/etc/thin/templates" </> name
  he <- liftIO $ doesFileExist homePath
  if he
    then do
         content <- liftIO $ readFile homePath
         return (homePath, content)
    else do
         ee <- liftIO $ doesFileExist etcPath
         if ee
           then do
                content <- liftIO $ readFile etcPath
                return (etcPath, content)
           else failure $ "Template not found neither in /etc/thin nor in ~: " ++ name
