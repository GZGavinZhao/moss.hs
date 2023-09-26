import Codec.Stone
import Criterion.Main
import Data.Binary
import Data.ByteString.Lazy qualified as LBS

main =
  defaultMain
    [ bgroup
        "read"
        [ bench "read bash-completion" $ nfIO (readStone "test/data/bash-completion-2.11-1-1-x86_64.stone"),
          bench "read linux-firmware" $ nfIO (readStone "test/data/linux-firmware-20230625-10-1-x86_64.stone")
        ]
    ]

readStone :: String -> IO Stone
readStone path = decode <$> LBS.readFile path
