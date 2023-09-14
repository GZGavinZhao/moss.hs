{-# LANGUAGE MagicHash #-}

module Codec.Stone.Utils.Decode where

import Control.Monad
import Data.Binary
import GHC.Exts
import GHC.Word

decodeWord8List :: Int -> Get [Word8]
decodeWord8List len = replicateM len getWord8

wordToWord32 :: Word -> Word32
wordToWord32 (W# word) = W32# (wordToWord32# word)

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
