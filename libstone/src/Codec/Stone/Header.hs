{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Codec.Stone.Header (Header(..)) where

import Codec.Stone.Header.AgnosticHeader
import Control.Exception
import Data.Binary
import Data.ByteString qualified as BS

stoneMagic :: BS.ByteString
stoneMagic = "\0mos"

{-# HLINT ignore "Use newtype instead of data" #-}
data Header = V1 AgnosticHeader
  deriving (Show, Eq)

instance Binary Header where
  put (V1 header) = put header
  get = do
    header <- get
    assert (magic header == stoneMagic) $ case version header of
      1 -> return (V1 header)
      ver -> error ("Unimplemented version: " ++ show ver)
