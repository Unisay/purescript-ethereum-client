module Data.Ethereum.Abi.Type.SignedInt
  ( SignedInt
  , mkSignedInt
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


-- | int<M>: two’s complement signed integer type of M bits, 0 < M <= 256, M % 8 == 0
data SignedInt m = SignedInt m BigInt

instance abiTypeSignedInt :: Dividend8 m => AbiType (SignedInt m) where
  -- int<M>: enc(X) is the big-endian two’s complement encoding of X,
  -- padded on the higher-oder (left) side with 0xff for negative X
  -- and with zero bytes for positive X such that the length is a multiple of 32 bytes.
  enc (SignedInt m i) = -- TODO
    let s = toBase 16 i
        lenInNibbles = S.length s
        align n = let r = n `mod` 64 in if r == 0 then n else n + 64 - r
        nibblesToPad = align (lenInNibbles) - lenInNibbles
        padding = joinWith "" $ A.replicate nibblesToPad "0"
    in "0x" <> padding <> s

instance eqSignedInt :: Dividend8 m => Eq (SignedInt m) where
  eq (SignedInt m l) (SignedInt _ r) = eq l r

instance showSignedInt :: Dividend8 m => Show (SignedInt m) where
  show (SignedInt m i) = "SignedInt " <> show (toInt m) <> " " <> toString i

instance toHexSignedInt :: Dividend8 m => ToHex (SignedInt m) where
  toHex (SignedInt _ i) = toHex i

instance fromHexSignedInt :: Dividend8 m => FromHex (SignedInt m) where
  fromHex s = fromHex s >>= mkSignedInt undefined

instance semiringSignedInt :: Dividend8 m => Semiring (SignedInt m) where
  zero = SignedInt undefined zero
  one = SignedInt undefined one
  mul (SignedInt m l) (SignedInt _ r) = SignedInt m (l * r)
  add (SignedInt m l) (SignedInt _ r) = SignedInt m (l + r)

instance arbitrarySignedInt8 :: Arbitrary (SignedInt D8) where
  arbitrary = SignedInt d8 <<< fromInt <$> chooseInt 0 255

instance arbitrarySignedInt16 :: Arbitrary (SignedInt (D1 :* D6)) where
  arbitrary = SignedInt d16 <<< fromInt <$> chooseInt 0 65535


-- | Signed n-bit integer: [0, 2^n)
mkSignedInt :: ∀ m. Dividend8 m => m -> BigInt -> Either String (SignedInt m)
mkSignedInt m i | i < zero =
  Left $ "SignedInt " <> show (toInt m)
                        <> " can't hold a negative value "
                        <> show i
mkSignedInt m i =
  let b = (fromInt 2) `pow` (fromInt $ toInt m)
  in if b <= i
     then Left $ "SignedInt " <> show (toInt m)
                                <> " can't hold a value greater than or equal to "
                                <> show b
     else Right $ SignedInt m i
