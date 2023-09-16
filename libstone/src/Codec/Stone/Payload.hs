{-# LANGUAGE TemplateHaskell #-}

module Codec.Stone.Payload where

import Codec.Stone.Payload.Header
import Codec.Stone.Payload.Kind qualified as K
import Codec.Stone.Payload.Meta qualified as M
import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Development.Placeholders

data Record
  = Meta M.Tag M.Kind

getRecord :: K.Kind -> Get Record
getRecord K.Meta = do
  len <- getWord32be
  tag <- toEnum . fromIntegral <$> getWord16be
  kindIdx <- toEnum . fromIntegral <$> getWord8
  skip 1
  kind <- case kindIdx of
    1 -> M.Int8 . fromIntegral <$> getWord8
    _ -> error "Wtf"
  return $ Meta tag kind

data Payload = Payload
  { header :: Header,
    records :: [Record]
  }

instance Binary Payload where
  put _ = $notImplemented
  get = do
    header <- get
    let cnt = fromIntegral $ numRecords header
    Payload header <$> replicateM cnt (getRecord $ kind header)
