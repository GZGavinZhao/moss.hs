{-# LANGUAGE TemplateHaskell #-}

module Codec.Stone where

import Codec.Stone.Header
import Codec.Stone.Payload
import Control.DeepSeq (NFData)
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Development.Placeholders
import GHC.Generics (Generic)

data Stone = Stone Header [Payload]
  deriving (Show, Eq, Generic, NFData)

instance Binary Stone where
  put = $notImplemented
  get = do
    header <- get
    case header of
      (V1 _ payloadCnt _) -> do
        Stone header <$> replicateM payloadCnt get
