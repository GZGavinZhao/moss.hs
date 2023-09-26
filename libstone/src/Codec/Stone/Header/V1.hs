{-# LANGUAGE TemplateHaskell #-}

module Codec.Stone.Header.V1 where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Development.Placeholders
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data FileType
  = Unknown
  | Binary
  | Delta
  | Repository
  | BuildManifest
  deriving (Show, Eq, Enum, Generic, NFData)

instance Binary FileType where
  put = $notImplemented
  get = toEnum . fromIntegral <$> getWord8

