{-# LANGUAGE DeriveDataTypeable #-}
module THIS.Templates.XML
  (XMLTemplateError (..),
   evalXMLFile
  ) where

import Control.Monad.Trans
import Control.Exception
import Control.Failure
import Data.Generics
import Data.Object
import Text.XML.HXT.Core

import THIS.Types
import THIS.Yaml
import THIS.Templates
import THIS.Templates.Text

data XMLTemplateError = XMLTemplateError String
  deriving (Eq, Show, Data, Typeable)

instance Exception XMLTemplateError

evalXMLFile :: StringObject -> [(String, String)] -> FilePath -> YamlM FilePath
evalXMLFile object vars name = do
  (path, tpl) <- readTemplate name
  tempPath <- liftIO $ tempFile
  liftIO $ processXML path tpl object vars tempPath
  return tempPath

processXML :: FilePath -> String -> StringObject -> [(String, String)] -> FilePath -> IO ()
processXML path tpl object vars dst = do
  runX $
    readString [withValidate yes, withInputEncoding utf8] tpl
    >>>
    evalXML path tpl object vars
    >>>
    writeDocument [withIndent yes, withOutputEncoding utf8] dst
  return ()

evalXML :: FilePath -> String -> StringObject -> [(String, String)] -> IOStateArrow s XmlTree XmlTree
evalXML path tpl object vars =
    processTopDownWithAttrl $ changeText go `when` isText
  where
    go t = case evalTemplate path object vars t of
             Left err -> throw (XMLTemplateError err)
             Right x -> x

