module Codec.Stone.Payload.Compression where

data Compression = None | Zstd deriving (Eq, Show)

instance Enum Compression where
  fromEnum None = 1
  fromEnum Zstd = 2

  toEnum 1 = None
  toEnum 2 = Zstd
