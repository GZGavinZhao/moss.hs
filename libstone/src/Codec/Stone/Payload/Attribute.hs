module Codec.Stone.Payload.Attribute where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Attribute = Attribute
  { key :: !BS.ByteString,
    value :: !BS.ByteString
  }
  deriving (Show, Eq, Generic, NFData)

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
