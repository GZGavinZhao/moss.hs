module Codec.Stone.Payload.Layout where

import Data.Binary
import Data.Binary.Get
import Data.Text qualified as T
import Data.WideWord (Word128)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Entry
  = Regular !BS.ByteString !T.Text
  | Symlink !T.Text !T.Text
  | Directory !T.Text
  | CharacterDevice !T.Text
  | BlockDevice !T.Text
  | Fifo !T.Text
  | Socket !T.Text
  deriving (Show, Eq, Generic, NFData)
