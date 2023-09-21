module Codec.Stone.Payload.Header where

import Codec.Stone.Payload.Compression
import Codec.Stone.Payload.Kind
import Codec.Stone.Utils.Decode
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.Word

data Header = Header
  { storedSize :: Word64,
    plainSize :: Word64,
    checksum :: BS.ByteString,
    numRecords :: Word,
    version :: Word16,
    kind :: Kind,
    compression :: Compression
  }
  deriving (Show, Eq)

instance Binary Header where
  get = do
    storedSize <- getWord64be
    plainSize <- getWord64be
    checksum <- getByteString 8
    numRecords <- fromIntegral <$> getWord32be
    version <- getWord16be
    kind <- toEnum . fromIntegral <$> getWord8
    compression <- toEnum . fromIntegral <$> getWord8
    return $ Header storedSize plainSize checksum numRecords version kind compression

  put Header {..} = do
    putWord64be storedSize
    putWord64be plainSize
    putByteString checksum
    putWord32be $ fromIntegral numRecords
    putWord16be version
    putWord8 $ fromIntegral $ fromEnum kind
    putWord8 $ fromIntegral $ fromEnum compression
