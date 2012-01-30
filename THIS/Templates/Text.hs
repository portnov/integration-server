{-# LANGUAGE ScopedTypeVariables #-}
module THIS.Templates.Text
  ( Item (..),
    parseTemplate,
    renderTemplate,
    evalTemplate,
    evalTextFile
  ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Data.Maybe
import Data.Object
import Data.Object.Yaml
import Text.Parsec
import Text.Parsec.String

import THIS.Types
import THIS.Yaml
import THIS.Templates

data Item =
    Literal String
  | Variable String String
  | Lookup String String String
  deriving (Eq, Show)

identifier :: Parser String
identifier = (many1 $ noneOf "[]{}$:? \t\n\r") <?> "identifier"

pVariable :: Parser Item
pVariable = do
  char '$'
  char '{'
  name <- identifier
  c <- oneOf "}[?"
  case c of
    '?' -> do
           def <- many1 $ noneOf "}"
           char '}'
           return (Variable name def)
    '}' -> return (Variable name "")
    '[' -> do
           key <- identifier
           char ']'
           e <- oneOf "}?"
           case e of
             '}' -> return (Lookup name key "")
             '?' -> do
                    def <- many1 $ noneOf "}"
                    char '}'
                    return (Lookup name key def)
             _ -> fail $ "Unexpected: " ++ [e]
    _ -> fail $ "Unexpected: " ++ [c]

pPlain :: Parser Item
pPlain = do
  text <- many1 $ noneOf "$"
  return (Literal text)

pTemplate :: Parser [Item]
pTemplate = many $ try pVariable <|> pPlain

parseTemplate :: FilePath -> String -> Either YamlError [Item]
parseTemplate path str =
  case parse pTemplate path str of
    Left err -> Left (show err)
    Right tpl -> Right tpl

renderTemplate :: StringObject -> [(String, String)] -> [Item] -> String
renderTemplate object pairs list = concatMap go list
  where
    go (Literal str) = str
    go (Variable var def) = fromMaybe def $ lookup var pairs
    go (Lookup dict key def) =
        case lookupYaml object pairs dict key of
          Left _ -> def
          Right val -> val

    lookupYaml :: StringObject -> [(String, String)] -> String -> String -> Either YamlError String
    lookupYaml object vars dictname keyname = do
      dict <- get dictname object :: Either YamlError StringObject
      let key = fromMaybe "" $ lookup keyname vars
      get key dict `mplus` get "$$" dict

evalTemplate :: FilePath -> StringObject -> [(String, String)] -> String -> Either YamlError String
evalTemplate path object pairs template = do
  list <- parseTemplate path template
  return $ renderTemplate object pairs list

evalTextFile :: StringObject -> [(String, String)] -> FilePath -> YamlM FilePath
evalTextFile object vars name = do
  (path, template) <- readTemplate name
  result <- ErrorT $ return $ evalTemplate path object vars template
  tempPath <- liftIO $ tempFile
  liftIO $ writeFile tempPath result
  return tempPath

