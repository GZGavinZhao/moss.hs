module Codec.Stone.HeaderSpec where

import Codec.Stone.Header
import Codec.Stone.Header.AgnosticHeader
import Codec.Stone.Header.V1 qualified as V1 (FileType (..))
import Data.Binary
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec

spec :: Spec
spec = do
  describe "Codec.Stone.Header" $ do
    it "can parse AgnosticHeaders" $ do
      stone <- LBS.readFile "test/data/bash-completion-2.11-1-1-x86_64.stone"
      let header = decode stone :: Header
      case header of
        (V1 agheader payloadCnt fileType) -> do
          version agheader `shouldBe` 1
          payloadCnt `shouldBe` 4
          fileType `shouldBe` V1.Binary
