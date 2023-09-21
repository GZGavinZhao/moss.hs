module Codec.Stone.Payload.Layout where

import Data.Binary
import Data.Binary.Get
import Data.Text qualified as T
import Data.WideWord (Word128)
import Data.ByteString qualified as BS

-- data FileType
--   = Regular
--   | Symlink
--   | Directory
--   | CharacterDevice
--   | BlockDevice
--   | Fifo
--   | Socket
--   deriving (Show, Eq)

data Entry
  = Regular BS.ByteString T.Text
  | Symlink T.Text T.Text
  | Directory T.Text
  | CharacterDevice T.Text
  | BlockDevice T.Text
  | Fifo T.Text
  | Socket T.Text
  deriving (Show, Eq)

-- getEntry :: Int -> Get Entry
-- getEntry 1 = Regular <$> getByteString 
-- getEntry 2 = Symlink <$> (T.decodeUtf8 <$> getByteString sourceLen) <*> (T.decodeUtf8 <$> getByteString targetLen)
