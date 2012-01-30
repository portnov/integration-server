{-# LANGUAGE ScopedTypeVariables #-}
module THIS.Templates.Text
  ( Item (..),
    parseTemplate,
    renderTemplate,
    evalTemplate
  ) where

import Control.Monad
import Data.Maybe
import Data.Object
import Data.Object.Yaml
import Text.Parsec
import Text.Parsec.String

import THIS.Types
import THIS.Yaml

data Item =
    Literal String
  | Variable String
  | Lookup String String
  deriving (Eq, Show)

pVariable :: Parser Item
pVariable = do
  char '$'
  char '{'
  name <- many1 $ noneOf "[]{}$: \t\n\r"
  c <- char '}' <|> char '['
  case c of
    '}' -> return (Variable name)
    '[' -> do
           key <- many1 $ noneOf "[]{}$: \t\n\r"
           char ']'
           char '}'
           return (Lookup name key)

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
    go (Variable var) = fromMaybe "" $ lookup var pairs
    go (Lookup dict key) = case lookupYaml object pairs dict key of
                             Left _ -> ""
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

