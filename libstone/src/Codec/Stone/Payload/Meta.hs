module Codec.Stone.Payload.Meta where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.Int
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text.Lazy.Builder.RealFloat (FPFormat(Generic))

data Dependency = PackageName | SharedLibrary | PkgConfig | Interpreter | CMake | Python | Binary | SystemBinary | PkgConfig32
  deriving (Show, Eq, Enum, Generic, NFData)

instance Binary Dependency where
  put dependency = putWord8 $ fromIntegral $ fromEnum dependency
  get = toEnum . fromIntegral <$> getWord8

showDependency :: Dependency -> String -> String
showDependency PackageName name = name
showDependency SharedLibrary name = name
showDependency PkgConfig name = "pkgconfig(" ++ name ++ ")"
showDependency Interpreter name = "interpreter(" ++ name ++ ")"
showDependency CMake name = "cmake(" ++ name ++ ")"
showDependency Python name = "python(" ++ name ++ ")"
showDependency Binary name = "binary(" ++ name ++ ")"
showDependency SystemBinary name = "system_binary(" ++ name ++ ")"
showDependency PkgConfig32 name = "pkgconfig32(" ++ name ++ ")"

data Kind
  = Int8 !Int8
  | Uint8 !Word8
  | Int16 !Int16
  | Uint16 !Word16
  | Int32 !Int32
  | Uint32 !Word32
  | Int64 !Int64
  | Uint64 !Word64
  | String !T.Text
  | Dependency !Dependency !T.Text
  | Provider !Dependency !T.Text
  deriving (Eq, Generic, NFData)

instance Show Kind where
  show (Int8 val) = show val
  show (Uint8 val) = show val
  show (Int16 val) = show val
  show (Uint16 val) = show val
  show (Int32 val) = show val
  show (Uint32 val) = show val
  show (Int64 val) = show val
  show (Uint64 val) = show val
  show (String text) = T.unpack text
  show (Dependency depType text) = showDependency depType (T.unpack text)
  show (Provider pdType text) = showDependency pdType (T.unpack text)

getKind :: Int -> Int -> Get Kind
getKind 1 _ = Int8 . fromIntegral <$> getWord8
getKind 2 _ = Uint8 . fromIntegral <$> getWord8
getKind 3 _ = Int16 . fromIntegral <$> getWord16be
getKind 4 _ = Uint16 . fromIntegral <$> getWord16be
getKind 5 _ = Int32 . fromIntegral <$> getWord32be
getKind 6 _ = Uint32 . fromIntegral <$> getWord32be
getKind 7 _ = Int64 . fromIntegral <$> getWord64be
getKind 8 _ = Uint64 . fromIntegral <$> getWord64be
getKind 9 len = String . T.decodeUtf8 <$> getByteString len
getKind 10 len = do
  depKind <- get
  Dependency depKind . T.decodeUtf8 <$> getByteString (len - 1)
getKind 11 len = do
  depKind <- get
  Provider depKind . T.decodeUtf8 <$> getByteString (len - 1)
getKind idx _ = error $ "getKind: unknown kind: " ++ show idx

data Tag
  = UnknownTag
  | Name
  | Architecture
  | Version
  | Summary
  | Description
  | Homepage
  | SourceID
  | Depends
  | Provides
  | Conflicts
  | Release
  | License
  | BuildRelease
  | PackageURI
  | PackageHash
  | PackageSize
  | BuildDepends
  | SourceURI
  | SourcePath
  | SourceRef
  deriving (Show, Eq, Enum, Generic, NFData)

instance Binary Tag where
  put tag = putWord16be $ fromIntegral $ fromEnum tag
  get = toEnum . fromIntegral <$> getWord16be
