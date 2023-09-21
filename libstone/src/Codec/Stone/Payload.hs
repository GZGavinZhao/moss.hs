{-# LANGUAGE TemplateHaskell #-}

module Codec.Stone.Payload where

import Codec.Stone.Payload.Header
import Codec.Stone.Payload.Kind qualified as K
import Codec.Stone.Payload.Layout qualified as L
import Codec.Stone.Payload.Meta qualified as M
import Codec.Stone.Utils.Decode (getWord128be)
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.WideWord.Word128
import Debug.Trace
import Development.Placeholders

data Record
  = Attribute BS.ByteString BS.ByteString
  | Index Int Int Word128
  | Meta M.Tag M.Kind
  | Layout Int Int Int Int L.Entry
  deriving (Show, Eq)

getRecord :: K.Kind -> Get Record
-- Meta
getRecord K.Meta = do
  len <- fromIntegral <$> getWord32be
  traceShowM len
  tag <- get
  traceShowM tag
  kindIdx <- toEnum . fromIntegral <$> getWord8
  skip 1
  kind <- M.getKind kindIdx len
  return $ Meta tag kind
-- Attribute
getRecord K.Attributes = do
  keyLen <- fromIntegral <$> getWord64be
  valueLen <- fromIntegral <$> getWord64be
  Attribute <$> getByteString keyLen <*> getByteString valueLen
-- Index
getRecord K.Index = Index <$> (fromIntegral <$> getWord64be) <*> (fromIntegral <$> getWord64be) <*> getWord128be
-- Layout
getRecord K.Layout = do
  uid <- fromIntegral <$> getWord32be
  gid <- fromIntegral <$> getWord32be
  mode <- fromIntegral <$> getWord32be
  tag <- fromIntegral <$> getWord32be

  sourceLen <- fromIntegral <$> getWord16be
  targetLen <- fromIntegral <$> getWord16be
  fileType <- fromIntegral <$> getWord8
  skip 11

  entry <- case fileType of
    1 -> L.Regular <$> getByteString sourceLen <*> (T.decodeUtf8 <$> getByteString targetLen)
    2 -> L.Symlink <$> (T.decodeUtf8 <$> getByteString sourceLen) <*> (T.decodeUtf8 <$> getByteString targetLen)
    3 -> L.Directory . T.decodeUtf8 <$> getByteString targetLen

  return $ Layout uid gid mode tag entry

data Payload = Payload
  { header :: Header,
    records :: [Record]
  }
  deriving (Show, Eq)

instance Binary Payload where
  put _ = $notImplemented
  get = do
    header <- get
    traceShowM header
    let cnt = fromIntegral $ numRecords header
    Payload header <$> replicateM cnt (getRecord $ kind header)
