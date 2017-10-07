module Data.Ethereum.Abi.Type.SignedInt
  ( SignedInt
  , mkSignedInt
  , invert
  , complement
  , isNegative
  ) where

import Prelude
import Data.Array as A
import Data.BigInt as I
import Data.String as S
import Data.BigInt (BigInt, xor)
import Data.Either (Either(Right, Left), note)
import Data.Ethereum.Abi.Class (class AbiType)
import Data.Ethereum.Abi.Type.Class (class Dividend8)
import Data.Maybe (fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.String (fromCharArray, joinWith)
import Data.Typelevel.Num (class Nat, type (:*), D1, D4, D6, D8, d16, d64, d8, toInt)
import Data.Typelevel.Undefined (undefined)
import Ethereum.Hex (class FromHex, class ToHex, stripHexPrefix, toHex)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, elements, vectorOf)


-- | int<M>: two’s complement signed integer type of M bits, 0 < M <= 256, M % 8 == 0
data SignedInt m = SignedInt m Boolean BigInt

instance abiTypeSignedInt :: Dividend8 m => AbiType (SignedInt m) where
  -- int<M>: enc(X) is the big-endian two’s complement encoding of X,
  -- padded on the higher-oder (left) side with 0xff for negative X
  -- and with zero bytes for positive X such that the length is a multiple of 32 bytes.
  enc si@(SignedInt m negative i) =
    let encoded = stripHexPrefix (toHex si)
        numNibbles = S.length encoded
        align n = let r = n `mod` 64 in if r == 0 then n else n + 64 - r
        nibblesToPad = align numNibbles - numNibbles
        digit = if negative then "f" else "0"
        padding = joinWith "" $ A.replicate nibblesToPad digit
    in "0x" <> padding <> encoded

instance eqSignedInt :: Dividend8 m => Eq (SignedInt m) where
  eq (SignedInt m lb li) (SignedInt _ rb ri) = lb `eq` rb && li `eq` ri

instance showSignedInt :: Dividend8 m => Show (SignedInt m) where
  show (SignedInt m b i) = "SignedInt " <> show (toInt m) <>
                                    " " <> (if b then "negative" else "positive") <>
                                    " " <> I.toString i

instance toHexSignedInt :: Dividend8 m => ToHex (SignedInt m) where
  toHex (SignedInt _ _ i) = toHex i

instance fromHexSignedInt :: Dividend8 m => FromHex (SignedInt m) where
  fromHex s = do
    decoded <- note "Failed to decode a hexademical string as a signed int" $
                  I.fromBase 16 (stripHexPrefix s)
    uncomplemented <- mkSignedInt undefined decoded
    pure $ complement uncomplemented

instance semiringSignedInt :: Dividend8 m => Semiring (SignedInt m) where
  zero = SignedInt undefined false zero
  one = SignedInt undefined false one

  -- TODO: Fix operations!

  mul (SignedInt m false l) (SignedInt _ false r) = SignedInt m false (l * r)
  mul (SignedInt m true l)  (SignedInt _ true r)  = SignedInt m false (l * r)
  mul (SignedInt m true l)  (SignedInt _ false r) = SignedInt m false (l * r)
  mul (SignedInt m false l) (SignedInt _ true r)  = SignedInt m false (l * r)

  add (SignedInt m false l) (SignedInt _ false r) = SignedInt m false (l + r)
  add (SignedInt m true l)  (SignedInt _  true r) = SignedInt m false (l + r)
  add (SignedInt m true l)  (SignedInt _ false r) = SignedInt m false (l + r)
  add (SignedInt m false l) (SignedInt _ true r)  = SignedInt m false (l + r)

-- | Signed n-bit integer: [−(2 `pow` n−1), (2 `pow` n−1))
mkSignedInt :: ∀ m. Dividend8 m => m -> BigInt -> Either String (SignedInt m)
mkSignedInt m = withBounds (lowerBound m) (upperBound m) where
  withBounds l u i
    | i < l = Left $ "SignedInt " <> show (toInt m) <> " can't hold a value "
                                  <> I.toString i <> " < " <> I.toString l
    | i > u = Left $ "SignedInt " <> show (toInt m) <> " can't hold a value "
                                  <> I.toString i <> " >= " <> I.toString u
    | i < zero = Right <<< complement <<< SignedInt m true $ i
    | otherwise = Right $ SignedInt m false i

upperBound :: ∀ n. Nat n => n -> BigInt
upperBound n = ((I.fromInt 2) `I.pow` I.fromInt (toInt n - 1)) - one

lowerBound :: ∀ n. Nat n => n -> BigInt
lowerBound n = negate $ I.fromInt 2 `I.pow` I.fromInt (toInt n - 1)

isNegative :: ∀ a. Dividend8 a => SignedInt a -> Boolean
isNegative (SignedInt _ negative _) = negative

invert :: ∀ a. Dividend8 a => SignedInt a -> SignedInt a
invert (SignedInt m b i) =
  let two = I.fromInt 2
      maxBits = I.fromInt (toInt m)
      inverted = xor i (I.pow two maxBits)
  in SignedInt m b inverted

complement :: ∀ a. Dividend8 a => SignedInt a -> SignedInt a
complement = invert >>> add one
