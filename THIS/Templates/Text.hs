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

defaultValue :: Parser String
defaultValue = (many1 $ noneOf "}") <?> "variable default value"

pVariable :: Parser Item
pVariable = do
  char '$'
  char '{'
  name <- identifier
  c <- oneOf "}[?"
  case c of
    '?' -> do
           def <- defaultValue
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
                    def <- defaultValue
                    char '}'
                    return (Lookup name key def)
             _ -> fail $ "Unexpected: " ++ [e]
    _ -> fail $ "Unexpected: " ++ [c]

pPlain :: Parser Item
pPlain = do
  text <- (many1 $ noneOf "$") <?> "any text without dollar signs"
  return (Literal text)

pTwoDollars :: Parser Item
pTwoDollars = do
  string "$$" <?> "two dollar signs"
  return (Literal "$")

pDollarChar :: Parser Item
pDollarChar = do
  char '$'
  x <- anyChar
  return (Literal ['$', x])

pDollarEnd :: Parser Item
pDollarEnd = do
  char '$'
  eof
  return (Literal "$")

pTemplate :: Parser [Item]
pTemplate = many $ try pVariable <|> try pPlain <|> try pTwoDollars <|> try pDollarChar <|> pDollarEnd

parseTemplate :: FilePath -> String -> Either YamlError [Item]
parseTemplate path str =
  if '$' `elem` str
    then case parse pTemplate path str of
           Left err -> Left (show err)
           Right tpl -> Right tpl
    else Right [Literal str]

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

