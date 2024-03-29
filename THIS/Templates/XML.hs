{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
-- | XML templates. Syntax is the same as in THIS.Templates.Text.
module THIS.Templates.XML
  (XMLTemplateError (..),
   evalXMLFile
  ) where

import Control.Monad.Trans
import Control.Exception
import Data.Generics
import Data.Object
import Text.XML.HXT.Core

import THIS.Types
import THIS.Templates
import THIS.Templates.Text

-- | Exception trhown on invalid template syntax
data XMLTemplateError = XMLTemplateError String
  deriving (Eq, Show, Data, Typeable)

instance Exception XMLTemplateError

-- | Evaluate XML template taken from file.
-- Returns rendered XML string.
evalXMLFile :: StringObject
            -> Variables
            -> FilePath    -- ^ Template file path
            -> THIS String
evalXMLFile object vars name = do
  (path, tpl) <- readTemplate name
  s <- liftIO $ processXML path tpl object vars
  return (concat s)

processXML :: FilePath -> String -> StringObject -> [(String, String)] -> IO [String]
processXML path tpl object vars = do
  runX $
    readString [withValidate yes, withInputEncoding utf8] tpl
    >>>
    evalXML path tpl object vars
    >>>
    writeDocumentToString [withIndent yes, withOutputEncoding utf8]

evalXML :: FilePath -> String -> StringObject -> [(String, String)] -> IOStateArrow s XmlTree XmlTree
evalXML path tpl object vars =
    processTopDownWithAttrl $ changeText go `when` isText
  where
    go t = case evalTemplate path object vars t of
             Left err -> throw (XMLTemplateError $ show err)
             Right x -> x

