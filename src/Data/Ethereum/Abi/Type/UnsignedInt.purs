module Data.Ethereum.Abi.Type.UnsignedInt
  ( UnsignedInt
  , mkUnsignedInt
  , Bool
  , mkBool
  ) where

import Prelude
import Data.Array as A
import Data.String as S
import Data.BigInt (BigInt, fromInt, pow, toBase, toString)
import Data.Ethereum.Abi.Class (class AbiType)
import Data.Ethereum.Abi.Type.Class (class Dividend8)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Typelevel.Num (type (:*), D1, D6, D8, d16, d8, toInt)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)


-- | uint<M>: unsigned integer type of M bits, 0 < M <= 256, M % 8 == 0
data UnsignedInt m = UnsignedInt m BigInt

instance abiTypeUnsignedInt :: Dividend8 m => AbiType (UnsignedInt m) where
  isStatic _ = true
  -- uint<M>: enc(X) is the big-endian encoding of X,
  -- padded on the higher-order (left) side with zero-bytes
  -- such that the length is a multiple of 32 bytes.
  enc (UnsignedInt m i) =
    let s = toBase 16 i
        lenInNibbles = S.length s
        align n = let r = n `mod` 64 in if r == 0 then n else n + 64 - r
        nibblesToPad = align (lenInNibbles) - lenInNibbles
        padding = joinWith "" $ A.replicate nibblesToPad "0"
    in "0x" <> padding <> s

instance showUnsignedInt :: Dividend8 m => Show (UnsignedInt m) where
  show (UnsignedInt m i) = "UnsignedInt " <> show (toInt m) <> " " <> toString i

instance arbitraryUnsignedInt8 :: Arbitrary (UnsignedInt D8) where
  arbitrary = UnsignedInt d8 <<< fromInt <$> chooseInt 0 255

instance arbitraryUnsignedInt16 :: Arbitrary (UnsignedInt (D1 :* D6)) where
  arbitrary = UnsignedInt d16 <<< fromInt <$> chooseInt 0 65535


-- | Unsigned n-bit integer: [0, 2^n)
mkUnsignedInt :: ∀ m. Dividend8 m => m -> BigInt -> Maybe (UnsignedInt m)
mkUnsignedInt m i
  | i >= zero && i < (fromInt 2) `pow` (fromInt $ toInt m) = Just $ UnsignedInt m i
  | otherwise = Nothing




newtype Bool = Bool (UnsignedInt D8)
-- instance abiTypeBool :: AbiType Bool where isStatic _ = true

mkBool :: Boolean -> Bool
mkBool false = Bool (UnsignedInt d8 zero)
mkBool true = Bool (UnsignedInt d8 one)
