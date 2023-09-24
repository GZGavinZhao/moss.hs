{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Codec.Stone.Payload (Record (..), Payload (..)) where

import Codec.Compression.Zstd.Lazy
import Codec.Stone.Payload.Compression
import Codec.Stone.Payload.Header
import Codec.Stone.Payload.Kind qualified as K
import Codec.Stone.Payload.Layout qualified as L
import Codec.Stone.Payload.Meta qualified as M
import Codec.Stone.Utils.Decode (getWord128be)
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.WideWord.Word128
import Debug.Pretty.Simple
import Development.Placeholders
import GHC.Int (Int64)

data Record
  = Attribute BS.ByteString BS.ByteString
  | Index Int Int Word128
  | Meta M.Tag M.Kind
  | Layout Int Int Int Int L.Entry
  | Content Int Int Int Compression
  | Dumb
  deriving (Show, Eq)

getRecord :: Header -> Get Record
-- Meta
getRecord Header {..} =
  case kind of
    -- Meta
    K.Meta -> do
      len <- fromIntegral <$> getWord32be
      tag <- get
      kindIdx <- toEnum . fromIntegral <$> getWord8
      skip 1
      kind <- M.getKind kindIdx len
      return $ Meta tag kind
    K.Attributes -> do
      keyLen <- fromIntegral <$> getWord64be
      valueLen <- fromIntegral <$> getWord64be
      Attribute <$> getByteString keyLen <*> getByteString valueLen
    -- Index
    K.Index -> do
      Index <$> (fromIntegral <$> getWord64be) <*> (fromIntegral <$> getWord64be) <*> getWord128be
    -- Layout
    K.Layout -> do
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
    K.Content -> do
      skip (fromIntegral plainSize)
      return $ Content (fromIntegral plainSize) (-1) (fromIntegral storedSize) compression

data Payload = Payload
  { header :: !Header,
    records :: ![Record]
  }
  deriving (Show, Eq)

getRecords :: Header -> Get [Record]
getRecords header = do
  empty <- isEmpty
  if empty
    then return []
    else do
      record <- getRecord header
      records <- getRecords header
      return (record : records)

instance Binary Payload where
  put _ = $notImplemented
  get = do
    header <- get
    let payloadKind = kind header
    rawRecords <- decompress <$> getLazyByteString (fromIntegral $ storedSize header)
    let decodedLen = fromIntegral $ plainSize header

    case kind header of
      K.Content ->
        do
          offset <- fromIntegral <$> bytesRead
          let record = runGet (pure $ Content (fromIntegral $ plainSize header) offset (fromIntegral $ storedSize header) (compression header)) rawRecords
          return $ Payload header [record]
      _ -> do
        let records = runGet (isolate decodedLen $ getRecords header) rawRecords
        return $ Payload header records
