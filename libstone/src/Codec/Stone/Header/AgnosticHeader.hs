module Codec.Stone.Header.AgnosticHeader (AgnosticHeader (..)) where

import Control.DeepSeq
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import GHC.Generics

data AgnosticHeader = AgnosticHeader
  { magic :: !BS.ByteString,
    data_ :: !BS.ByteString,
    version :: !Word32
  }
  deriving (Show, Eq, Generic, NFData)

instance Binary AgnosticHeader where
  put AgnosticHeader {..} = putByteString magic >> putByteString data_ >> putWord32be version
  get = getAgnosticHeader

getAgnosticHeader :: Get AgnosticHeader
getAgnosticHeader = AgnosticHeader <$> getByteString 4 <*> getByteString 24 <*> getWord32be
