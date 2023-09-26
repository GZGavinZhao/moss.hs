module Codec.Stone.Payload.Compression where
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Compression = None | Zstd deriving (Eq, Show, Generic, NFData)

instance Enum Compression where
  fromEnum None = 1
  fromEnum Zstd = 2

  toEnum 1 = None
  toEnum 2 = Zstd
