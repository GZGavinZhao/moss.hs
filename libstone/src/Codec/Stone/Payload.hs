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
  len <- fromIntegral <$> getWord32be
  tag <- get
  kindIdx <- toEnum . fromIntegral <$> getWord8
  skip 1
  kind <- M.getKind kindIdx len
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
