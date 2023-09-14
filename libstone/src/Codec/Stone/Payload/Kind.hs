module Codec.Stone.Payload.Kind where

data Kind
  = Meta
  | Content
  | Layout
  | Index
  | Attributes
  | Dumb
  deriving (Show, Eq)

instance Enum Kind where
  toEnum x = case x of
    1 -> Meta
    2 -> Content
    3 -> Layout
    4 -> Index
    5 -> Attributes
    6 -> Dumb

  fromEnum kind = case kind of
    Meta -> 1
    Content -> 2
    Layout -> 3
    Index -> 4
    Attributes -> 5
    Dumb -> 6
