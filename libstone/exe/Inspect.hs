{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import Codec.Stone
import Codec.Stone.Payload
import Codec.Stone.Payload.Header
import Codec.Stone.Payload.Kind qualified as K
import Codec.Stone.Payload.Layout qualified as L
import Codec.Stone.Payload.Meta qualified as M
import Control.Monad
import Data.Binary
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Development.Placeholders (notImplemented)
import Options.Applicative.Simple
import Paths_libstone qualified as Meta
import Text.Pretty.Simple
import Text.Printf

data Sample = Option
  { filePath :: String
  }

sample :: Parser Sample
sample =
  Option
    <$> argument str (metavar "FILE")

main :: IO ()
main = do
  (opts, ()) <-
    simpleOptions
      $(simpleVersion Meta.version)
      "Stone container format inspection tool, written in Haskell"
      "Inspect a .stone file's content"
      sample
      empty
  dumpStone opts

dumpStone :: Sample -> IO ()
dumpStone Option {..} = do
  (Stone _ payloads) <- decode <$> LBS.readFile filePath
  forM_ payloads $ \payload -> do
    dumpPayload payload

showEntry :: L.Entry -> String
showEntry (L.Regular checksum path) = T.unpack path ++ " - [Regular]"
showEntry (L.Symlink to from) = T.unpack from ++ " -> " ++ T.unpack to ++ " [Symlink]"
showEntry (L.Directory path) = T.unpack path ++ " [Directory]"
showEntry _ = $notImplemented

toHexString :: BS.ByteString -> String
toHexString = BS.foldr ((<>) . printf "%02x") ""

dumpPayload :: Payload -> IO ()
dumpPayload Payload {..} =
  case kind header of
    K.Meta -> forM_ records $ \(Meta tag kind) -> do
      putStrLn $ show tag ++ ": " ++ show kind
    K.Layout -> forM_ records $ \(Layout _ _ _ _ entry) -> putStrLn $ "\t- " ++ showEntry entry
    _ -> putStrLn "Some other payload"
