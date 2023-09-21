{-# LANGUAGE MagicHash #-}

module Codec.Stone.Utils.Decode where

import Control.Monad
import Data.Binary
import Data.Binary.Get.Internal
import Data.Bits
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as B
import Data.WideWord.Word128
import GHC.Exts
import GHC.Word

decodeWord8List :: Int -> Get [Word8]
decodeWord8List len = replicateM len getWord8

wordToWord32 :: Word -> Word32
wordToWord32 (W# word) = W32# (wordToWord32# word)

-- | Read a Word128 in big endian format
getWord128be :: Get Word128
getWord128be = readN 16 word128be

word128be :: B.ByteString -> Word128
word128be s =
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 120)
    .|. (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 112)
    .|. (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 104)
    .|. (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 96)
    .|. (fromIntegral (s `B.unsafeIndex` 4) `unsafeShiftL` 88)
    .|. (fromIntegral (s `B.unsafeIndex` 5) `unsafeShiftL` 80)
    .|. (fromIntegral (s `B.unsafeIndex` 6) `unsafeShiftL` 72)
    .|. (fromIntegral (s `B.unsafeIndex` 7) `unsafeShiftL` 64)
    .|. (fromIntegral (s `B.unsafeIndex` 8) `unsafeShiftL` 56)
    .|. (fromIntegral (s `B.unsafeIndex` 9) `unsafeShiftL` 48)
    .|. (fromIntegral (s `B.unsafeIndex` 10) `unsafeShiftL` 40)
    .|. (fromIntegral (s `B.unsafeIndex` 11) `unsafeShiftL` 32)
    .|. (fromIntegral (s `B.unsafeIndex` 12) `unsafeShiftL` 24)
    .|. (fromIntegral (s `B.unsafeIndex` 13) `unsafeShiftL` 16)
    .|. (fromIntegral (s `B.unsafeIndex` 14) `unsafeShiftL` 8)
    .|. fromIntegral (s `B.unsafeIndex` 15)

{-# INLINE [2] getWord128be #-}

{-# INLINE word128be #-}

-- The following code is from:

-- |
-- Module      : Codec.CBOR.Magic
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- An internal module for doing magical, low-level, and unholy things
-- in the name of efficiency.
{-# INLINE word32ToInt #-}
word32ToInt (W32# w#) = I# (word2Int# (word32ToWord# w#))

{-# INLINE word8ToInt #-}
word8ToInt (W8# w#) = I# (word2Int# (word8ToWord# w#))

{-# INLINE word32ToWord #-}
word32ToWord (W32# w#) = W# (word32ToWord# w#)
