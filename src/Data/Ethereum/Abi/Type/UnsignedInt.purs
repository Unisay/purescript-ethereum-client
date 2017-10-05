module Data.Ethereum.Abi.Type.UnsignedInt
  ( UnsignedInt
  , mkUnsignedInt
  ) where

import Prelude

import Data.Array as A
import Data.BigInt (BigInt, fromInt, pow, toBase, toString)
import Data.Either (Either(Right, Left))
import Data.Ethereum.Abi.Class (class AbiType)
import Data.Ethereum.Abi.Type.Class (class Dividend8)
import Data.String (joinWith)
import Data.String as S
import Data.Typelevel.Num (type (:*), D1, D6, D8, d16, d8, toInt)
import Data.Typelevel.Undefined (undefined)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)


-- | uint<M>: unsigned integer type of M bits, 0 < M <= 256, M % 8 == 0
data UnsignedInt m = UnsignedInt m BigInt

instance abiTypeUnsignedInt :: Dividend8 m => AbiType (UnsignedInt m) where
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

instance eqUnsignedInt :: Dividend8 m => Eq (UnsignedInt m) where
  eq (UnsignedInt m l) (UnsignedInt _ r) = eq l r

instance showUnsignedInt :: Dividend8 m => Show (UnsignedInt m) where
  show (UnsignedInt m i) = "UnsignedInt " <> show (toInt m) <> " " <> toString i

instance toHexUnsignedInt :: Dividend8 m => ToHex (UnsignedInt m) where
  toHex (UnsignedInt _ i) = toHex i

instance fromHexUnsignedInt :: Dividend8 m => FromHex (UnsignedInt m) where
  fromHex s = fromHex s >>= mkUnsignedInt undefined

instance semiringUnsignedInt :: Dividend8 m => Semiring (UnsignedInt m) where
  zero = UnsignedInt undefined zero
  one = UnsignedInt undefined one
  mul (UnsignedInt m l) (UnsignedInt _ r) = UnsignedInt m (l * r)
  add (UnsignedInt m l) (UnsignedInt _ r) = UnsignedInt m (l + r)

instance arbitraryUnsignedInt8 :: Arbitrary (UnsignedInt D8) where
  arbitrary = UnsignedInt d8 <<< fromInt <$> chooseInt 0 255

instance arbitraryUnsignedInt16 :: Arbitrary (UnsignedInt (D1 :* D6)) where
  arbitrary = UnsignedInt d16 <<< fromInt <$> chooseInt 0 65535


-- | Unsigned n-bit integer: [0, 2^n)
mkUnsignedInt :: ∀ m. Dividend8 m => m -> BigInt -> Either String (UnsignedInt m)
mkUnsignedInt m i | i < zero =
  Left $ "UnsignedInt " <> show (toInt m)
                        <> " can't hold a negative value "
                        <> show i
mkUnsignedInt m i =
  let b = (fromInt 2) `pow` (fromInt $ toInt m)
  in if b <= i
     then Left $ "UnsignedInt " <> show (toInt m)
                                <> " can't hold a value greater than or equal to "
                                <> show b
     else Right $ UnsignedInt m i
