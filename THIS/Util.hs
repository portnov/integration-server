
module THIS.Util where

import Control.Monad
import Control.Monad.Error
import Data.Char
import qualified Data.ByteString as B
import System.Exit
import System.Random

import THIS.Types

-- | Get parser result name:
-- if `group' parameter is defined, use it;
-- otherwise, use group name.
groupName :: ParserResult -> String
groupName pr =
  case lookup "group" (prParams pr) of
    Just group -> group
    Nothing    -> prGroupName pr

liftEither :: Either ErrorMessage a -> THIS a
liftEither e = ErrorT (return e)

liftEitherWith :: (e -> ErrorMessage) -> Either e a -> THIS a
liftEitherWith fn x =
  case x of
    Left e  -> ErrorT (return $ Left $ fn e)
    Right v -> ErrorT (return $ Right v)

liftError :: (e -> e') -> Either e a -> Either e' a
liftError fn (Left e)  = Left (fn e)
liftError _  (Right v) = Right v

toBS :: String -> B.ByteString
toBS s = B.pack $ map (fromIntegral . ord) s

fromBS :: B.ByteString -> String
fromBS bs = map (chr . fromIntegral) $ B.unpack bs

rc2int :: ExitCode -> Int
rc2int ExitSuccess = 0
rc2int (ExitFailure n) = n

tempFile :: IO FilePath
tempFile = do
  r <- replicateM 5 $ randomRIO ('0', '9')
  return $ "/tmp/this" ++ r

