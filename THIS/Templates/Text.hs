{-# LANGUAGE ScopedTypeVariables #-}
-- | Text templates. Supported substitutions are:
--
--  * ${variable}
--  * ${variable?default-value}
--  * ${dictionary[key]}
--  * ${dictionary[key]?default-value}
--
module THIS.Templates.Text
  ( Item (..),
    parseTemplate,
    renderTemplate,
    evalTemplate,
    evalTextFile
  ) where

import Control.Monad
import Control.Monad.Error
import Control.Failure hiding (try)
import Data.Maybe
import Data.Object
import Text.Parsec
import Text.Parsec.String as P

import THIS.Types
import THIS.Util
import THIS.Yaml
import THIS.Templates

-- | Template item
data Item =
    Literal String              -- ^ string literal
  | Variable String String      -- ^ variable subsitiution: variable name, default value
  | Lookup String String String -- ^ dictionary lookup: dictionary name, key, default value
  deriving (Eq, Show)

identifier :: P.Parser String
identifier = (many1 $ noneOf "[]{}$:? \t\n\r") <?> "identifier"

defaultValue :: P.Parser String
defaultValue = (many1 $ noneOf "}") <?> "variable default value"

pVariable :: P.Parser Item
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

pPlain :: P.Parser Item
pPlain = do
  text <- (many1 $ noneOf "$") <?> "any text without dollar signs"
  return (Literal text)

pTwoDollars :: P.Parser Item
pTwoDollars = do
  string "$$" <?> "two dollar signs"
  return (Literal "$")

pDollarChar :: P.Parser Item
pDollarChar = do
  char '$'
  x <- anyChar
  return (Literal ['$', x])

pDollarEnd :: P.Parser Item
pDollarEnd = do
  char '$'
  eof
  return (Literal "$")

pTemplate :: P.Parser [Item]
pTemplate = many $ try pVariable <|> try pPlain <|> try pTwoDollars <|> try pDollarChar <|> pDollarEnd

-- | Parse template
parseTemplate :: FilePath  -- ^ Template file path (used in error messages)
              -> String    -- ^ Template text
              -> Either ParseError [Item]
parseTemplate path str =
  if '$' `elem` str
    then case parse pTemplate path str of
           Left err -> failure err
           Right tpl -> return tpl
    else return [Literal str]

-- | Render template
renderTemplate :: StringObject -> Variables -> [Item] -> String
renderTemplate object pairs list = concatMap go list
  where
    go (Literal str) = str
    go (Variable var def) = fromMaybe def $ lookup var pairs
    go (Lookup dict key def) =
        case lookupYaml object pairs dict key def of
          Left _ -> def
          Right val -> val

    lookupYaml :: StringObject -> [(String, String)] -> String -> String -> String -> Either ErrorMessage String
    lookupYaml object vars dictname keyname def = do
      dict <- get dictname object :: Either ErrorMessage StringObject
      let key = fromMaybe "$$" $ lookup keyname vars
      get key dict `mplus` get "$$" dict `mplus` return def

-- | Evaluate template
evalTemplate :: FilePath     -- ^ Template file path (used in error messages)
             -> StringObject -- 
             -> Variables
             -> String       -- ^ Template itself
             -> Either ParseError String
evalTemplate path object pairs template = do
  list <- parseTemplate path template
  return $ renderTemplate object pairs list

-- | Evaluate template from text file.
-- Returns path to temporary file with rendered text.
evalTextFile :: StringObject
             -> Variables
             -> FilePath      -- ^ Source template path
             -> THIS FilePath
evalTextFile object vars name = do
  (path, template) <- readTemplate name
  result <- liftEitherWith ParsecError $ evalTemplate path object vars template
  tempPath <- liftIO $ tempFile
  liftIO $ writeFile tempPath result
  return tempPath

