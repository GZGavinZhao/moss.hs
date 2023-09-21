module Codec.StoneSpec where

import Codec.Stone
import Codec.Stone.Header
import Codec.Stone.Header.AgnosticHeader
import Data.Binary
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec

spec :: Spec
spec = do
  describe "Codec.Stone" $ do
    it "can parse a stone package file" $ do
      (Stone header payloads) <- decode <$> LBS.readFile "test/data/bash-completion-2.11-1-1-x86_64.stone" :: IO Stone
      length payloads `shouldBe` 4
