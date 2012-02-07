{-# LANGUAGE ScopedTypeVariables #-}
-- | Text templates. Supported substitutions are:
--
--  * ${variable}
--
--  * ${variable?default-value}
--
--  * ${dictionary[key]}
--
--  * ${dictionary[key]?default-value}
--
--  * ${dictionary[key]\/subkey}
--
module THIS.Templates.Text
  ( Template,
    Item (..),
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

import Debug.Trace

type Template = [Item]

-- | Template item
data Item =
    Literal String              -- ^ string literal
  | Variable String String      -- ^ variable subsitiution: variable name, default value
  | Lookup String Item String -- ^ dictionary lookup: dictionary name, key, default value
  | Item :/: Item
  deriving (Eq, Show)

identifier :: P.Parser String
identifier = (many1 $ noneOf "[]{}$:?/ \t\n\r") <?> "identifier"

defaultValue :: P.Parser String
defaultValue = (many $ noneOf "/}]") <?> "variable default value"

pKey :: P.Parser Item
pKey = do
  name <- identifier
  c <- oneOf "]?"
  case c of
    ']' -> return (Variable name "")
    '?' -> do
           def <- defaultValue
           char ']'
           return (Variable name def)
    _   -> fail $ "Unexpected: " ++ [c]

nested = do
  list <- pItem True `sepBy1` char '/'
  return $ foldr1 (\x y -> x :/: y) list

nest :: Item -> P.Parser Item
nest item = do
  next <- (try nested <|> pItem True) <?> "nested template"
  char '}'
  return $ item :/: next

pVariable :: P.Parser Item
pVariable = do
  string "${" <?> "variable start"
  name <- identifier
  c <- oneOf "}[?/"
  case c of
    '?' -> do
           def <- defaultValue
           char '}'
           return (Variable name def)
    '}' -> return (Variable name [])
    '[' -> do
           key <- pKey
           e <- oneOf "}?/"
           case e of
             '}' -> return (Lookup name key [])
             '?' -> do
                    def <- defaultValue
                    char '}'
                    return (Lookup name key def)
             '/' -> nest   (Lookup name key [])
             _ -> fail $ "Unexpected: " ++ [e]
    _ -> fail $ "Unexpected: " ++ [c]

pPlain :: Bool -> P.Parser Item
pPlain b = do
  let exc = if b then "$]}/" else "$"
  text <- (many1 $ noneOf exc) <?> "any text without dollar signs"
  return (Literal text)

pLiftedPlain :: P.Parser Item
pLiftedPlain = do
  let exc = "$]}/"
  text <- (many1 $ noneOf exc) <?> "any text without dollar signs"
  return (Variable text [])

pTwoDollars :: P.Parser Item
pTwoDollars = do
  string "$$" <?> "two dollar signs"
  return (Literal "$")

pDollarChar :: P.Parser Item
pDollarChar = do
  char '$' <?> "one dollar sign"
  x <- anyChar
  return (Literal ['$', x])

pDollarEnd :: P.Parser Item
pDollarEnd = do
  char '$' <?> "one dollar sign at end of input"
  eof
  return (Literal "$")

pItem :: Bool -> P.Parser Item
pItem b = try pVariable <|> try (pPlain b) <|> try pTwoDollars <|> try pDollarChar <|> pDollarEnd

pKeyItem :: P.Parser Item
pKeyItem = try pVariable <|> try pLiftedPlain <|> try pTwoDollars <|> try pDollarChar <|> pDollarEnd

pTemplate :: Bool -> P.Parser Template
pTemplate b = many (pItem b)

-- | Parse template
parseTemplate :: FilePath  -- ^ Template file path (used in error messages)
              -> String    -- ^ Template text
              -> Either ParseError Template
parseTemplate path str =
  if '$' `elem` str
    then case parse (pTemplate False) path str of
           Left err -> failure err
           Right tpl -> return tpl
    else return [Literal str]

-- | Render template
renderTemplate :: StringObject -> Variables -> Template -> Either ErrorMessage String
renderTemplate object vars list = render list
  where
    render l = concat `fmap` mapM (renderItem object vars) l

lookupYaml :: StringObject -> Variables -> String -> String -> String -> Either ErrorMessage String
lookupYaml object vars dictname key def = do
  dict <- get dictname object :: Either ErrorMessage StringObject
  get key dict `mplus` get "$$" dict `mplus` return def

renderItem :: StringObject -> Variables -> Item -> Either ErrorMessage String
renderItem _ _ (Literal str) = return str
renderItem o vars (Variable var def) = do
    objectVars <- getPairs o
    return $ fromMaybe def $
                 lookup var vars `mplus` lookup var objectVars
renderItem o vars (Lookup dict key def) = do
    keyR <- renderItem o vars key
    case lookupYaml o vars dict keyR def of
       Left _    -> return def
       Right val -> return val
renderItem o vars (parent :/: child) = do
  parentObject <- getD vars parent o
  parentVars <- getPairs parentObject
  let vars' = parentVars ++ vars
  renderItem parentObject vars' child

getD :: Variables -> Item -> StringObject -> Either ErrorMessage StringObject
getD extVars (Lookup name key def) (Mapping pairs) =
  case lookup name pairs of
    Nothing -> failure $ "No such key: " ++ name
    Just val@(Mapping internal) -> do
        vars <- getPairs val
        let vars' = vars ++ extVars
        keyR <- renderItem val vars' key
        case lookup keyR internal of
          Just result -> return result
          Nothing -> failure $ "No such key: " ++ name ++ "/" ++ keyR
    Just val -> failure $ "nesting can be applied only for dictionaries, not " ++ show val
getD _ i o = failure $ "nesting can be applied only for dictionary lookup, not "
                     ++ show i ++ ", " ++ show o

-- | Evaluate template
evalTemplate :: FilePath     -- ^ Template file path (used in error messages)
             -> StringObject -- 
             -> Variables
             -> String       -- ^ Template itself
             -> Either ErrorMessage String
evalTemplate path object vars template = do
  list <- liftError ParsecError $ parseTemplate path template
  renderTemplate object vars list

-- | Evaluate template from text file.
-- Returns path to temporary file with rendered text.
evalTextFile :: StringObject
             -> Variables
             -> FilePath      -- ^ Source template path
             -> THIS FilePath
evalTextFile object vars name = do
  (path, template) <- readTemplate name
  result <- liftEither $ evalTemplate path object vars template
  tempPath <- liftIO $ tempFile
  liftIO $ writeFile tempPath result
  return tempPath

