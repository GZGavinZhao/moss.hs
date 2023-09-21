module Codec.Stone.Payload.Attribute where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS

data Attribute = Attribute
  { key :: BS.ByteString,
    value :: BS.ByteString
  }
  deriving (Show, Eq)

instance Binary Attribute where
  put Attribute {..} = do
    putWord64be $ fromIntegral $ BS.length key
    putWord64be $ fromIntegral $ BS.length value
    putByteString key
    putByteString value

  get = do
    keyLen <- fromIntegral <$> getWord64be
    valueLen <- fromIntegral <$> getWord64be
    Attribute <$> getByteString keyLen <*> getByteString valueLen
