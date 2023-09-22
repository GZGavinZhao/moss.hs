{-# LANGUAGE RecordWildCards #-}

module Codec.StoneSpec where

import Codec.Stone
import Codec.Stone.Header
import Codec.Stone.Header.AgnosticHeader
import Codec.Stone.Payload
import Codec.Stone.Payload.Header
import Codec.Stone.Payload.Kind qualified as K
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec
import Text.Pretty.Simple

spec :: Spec
spec = do
  describe "Codec.Stone" $ do
    it "can parse a stone package file" $ do
      (Stone _ payloads) <- decode <$> LBS.readFile "test/data/bash-completion-2.11-1-1-x86_64.stone" :: IO Stone
      length payloads `shouldBe` 4
      forM_ payloads $ \Payload {..} -> do
        unless (kind header == K.Content) $
          length records `shouldBe` fromIntegral (numRecords header)
      -- pPrint $ map records payloads
