{-# LANGUAGE TemplateHaskell #-}

module Codec.Stone where

import Control.Monad
import Codec.Stone.Header
import Codec.Stone.Payload
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Development.Placeholders

data Stone = Stone Header [Payload]
  deriving (Show, Eq)

instance Binary Stone where
  put = $notImplemented
  get = do
    header <- get
    case header of
      (V1 _ payloadCnt _) -> do
        Stone header <$> replicateM payloadCnt get
