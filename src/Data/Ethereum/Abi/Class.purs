module Data.Ethereum.Abi.Class where

import Prelude

import Data.Array (replicate)
import Data.Binary (hex)
import Data.Binary as Bin
import Data.Binary.SignedInt (SignedInt)
import Data.Binary.UnsignedInt (UnsignedInt)
import Data.Ethereum.Abi.Type.Class (class Dividend8)
import Data.Ord (abs)
import Data.String (fromCharArray)
import Data.String as Str
import Data.Typelevel.Num as Nat
import Data.Typelevel.Undefined (undefined)


class AbiType a where
  enc :: a -> String

instance abiTypeBoolean :: AbiType Boolean where
  enc true  = "0x0000000000000000000000000000000000000000000000000000000000000001"
  enc false = "0x0000000000000000000000000000000000000000000000000000000000000000"

--
-- instance abiTypeString :: AbiType String where
--   isStatic _ = false
--   encode = ?x
--
-- instance abiTypeByteString :: AbiType ByteString where
--   isStatic _ = false
--   encode = ?x

instance abiTypeUnsignedInt :: Dividend8 m => AbiType (UnsignedInt m) where
  enc ui = "0x" <> padding <> encoded where
    padding = if delta > 0
              then fromCharArray (replicate delta '0')
              else ""
    delta = abs (width - len)
    width = Nat.toInt (undefined :: m)
    len = Str.length encoded
    encoded = Bin.toStringAs hex ui

instance abiTypeSignedInt :: Dividend8 m => AbiType (SignedInt m) where
  enc si = "0x" <> Bin.toStringAs hex si
