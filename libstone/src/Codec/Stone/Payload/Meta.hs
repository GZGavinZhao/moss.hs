module Codec.Stone.Payload.Meta where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Word

data Dependency = PackageName | SharedLibrary | PkgConfig | Interpreter | CMake | Python | Binary | SystemBinary | PkgConfig32
  deriving (Show, Eq, Enum)

instance Binary Dependency where
  put dependency = putWord8 $ fromIntegral $ fromEnum dependency
  get = toEnum . fromIntegral <$> getWord8

data Kind
  = Int8 Int8
  | Uint8 Word8
  | Int16 Int16
  | Uint16 Word16
  | Int32 Int32
  | Uint32 Word32
  | Int64 Int64
  | Uint64 Word64
  | String String
  | Dependency Dependency String
  | Provider Dependency String
  deriving (Show, Eq)

data Tag
  = Name
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
  deriving (Show, Eq)

instance Enum Tag where
  fromEnum Name = 1
  fromEnum Architecture = 2
  fromEnum Version = 3
  fromEnum Summary = 4
  fromEnum Description = 5
  fromEnum Homepage = 6
  fromEnum SourceID = 7
  fromEnum Depends = 8
  fromEnum Provides = 9
  fromEnum Conflicts = 10
  fromEnum Release = 11
  fromEnum License = 12
  fromEnum BuildRelease = 13
  fromEnum PackageURI = 14
  fromEnum PackageHash = 15
  fromEnum PackageSize = 16
  fromEnum BuildDepends = 17
  fromEnum SourceURI = 18
  fromEnum SourcePath = 19
  fromEnum SourceRef = 20

  toEnum 1 = Name
  toEnum 2 = Architecture
  toEnum 3 = Version
  toEnum 4 = Summary
  toEnum 5 = Description
  toEnum 6 = Homepage
  toEnum 7 = SourceID
  toEnum 8 = Depends
  toEnum 9 = Provides
  toEnum 10 = Conflicts
  toEnum 11 = Release
  toEnum 12 = License
  toEnum 13 = BuildRelease
  toEnum 14 = PackageURI
  toEnum 15 = PackageHash
  toEnum 16 = PackageSize
  toEnum 17 = BuildDepends
  toEnum 18 = SourceURI
  toEnum 19 = SourcePath
  toEnum 20 = SourceRef
  toEnum x = error $ "Unknown conversion to Tag: " ++ show x

instance Binary Tag where
  put tag = putWord16be $ fromIntegral $ fromEnum tag
  get = toEnum . fromIntegral <$> getWord16be

