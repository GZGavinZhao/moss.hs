{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Codec.Stone.Header (Header(..)) where

import Codec.Stone.Header.AgnosticHeader
import Codec.Stone.Header.V1
import Control.Exception
import Data.Binary
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Development.Placeholders

stoneMagic :: BS.ByteString
stoneMagic = "\0mos"

{-# HLINT ignore "Use newtype instead of data" #-}
data Header = V1 AgnosticHeader Int FileType
  deriving (Show, Eq)

getV1 :: Get (Int, FileType) = do
  payloadCnt <- fromIntegral <$> getWord16be
  integrity <- getByteString 21
  fileType <- get
  return (payloadCnt, fileType)

instance Binary Header where
  put = $notImplemented
  get = do
    header <- get
    let headerData = LBS.fromStrict $ data_ header

    assert (magic header == stoneMagic) $ case version header of
      1 -> 
        let (payloadCnt, fileType) = runGet getV1 headerData
        in return $ V1 header payloadCnt fileType
      ver -> error ("Unimplemented version: " ++ show ver)
