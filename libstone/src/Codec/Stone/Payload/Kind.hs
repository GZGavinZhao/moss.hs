module Codec.Stone.Payload.Kind where
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Kind
  = UnkownKind
  | Meta
  | Content
  | Layout
  | Index
  | Attributes
  | Dumb
  deriving (Show, Eq, Enum, Generic, NFData)
