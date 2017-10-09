module Data.Ethereum.Abi.Type.SignedInt
  ( SignedInt
  , mkSignedInt
  , invert
  , complement
  , isNegative
  , toBigInt
  , lowerBound
  , upperBound
  ) where

import Prelude
import Data.Array as A
import Data.BigInt as I
import Data.String as S
import Data.BigInt (BigInt, xor)
import Data.Either (Either(Right, Left))
import Data.Ethereum.Abi.Class (class AbiType)
import Data.Ethereum.Abi.Type.Class (class Dividend8)
import Data.Ethereum.Error (Errors, mkErrors, noteErrors)
import Data.String (joinWith)
import Data.Typelevel.Num (class Nat, toInt)
import Data.Typelevel.Undefined (undefined)
import Ethereum.Hex (class FromHex, class ToHex, stripHexPrefix, toHex)


-- | int<M>: two’s complement signed integer type of M bits, 0 < M <= 256, M % 8 == 0
data SignedInt m = SignedInt m Boolean BigInt

-- | unsafe because of possible overflow
unsafeFromBigInt :: ∀ m. Dividend8 m => m -> BigInt -> SignedInt m
unsafeFromBigInt m i = if i < zero then SignedInt m true (I.abs i)
                                   else SignedInt m false i

toBigInt :: ∀ m. Dividend8 m => SignedInt m -> BigInt
toBigInt (SignedInt _ negative value) =
  if negative then negate value else value

instance abiTypeSignedInt :: Dividend8 m => AbiType (SignedInt m) where
  -- int<M>: enc(X) is the big-endian two’s complement encoding of X,
  -- padded on the higher-oder (left) side with 0xff for negative X
  -- and with zero bytes for positive X such that the length is a multiple of 32 bytes.
  enc si@(SignedInt m negative i) =
    let encoded = (stripHexPrefix <<< toHex) si
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
  fromHex q = do
    decoded <- noteErrors "Failed to decode a hexademical string as a signed int" $
                I.fromBase 16 (stripHexPrefix q)
    uncomplemented <- mkSignedInt undefined decoded
    pure $ complement uncomplemented

instance semiringSignedInt :: Dividend8 m => Semiring (SignedInt m) where
  zero = SignedInt undefined false zero
  one = SignedInt undefined false one
  mul l r = unsafeFromBigInt undefined (toBigInt l * toBigInt r)
  add l r = unsafeFromBigInt undefined (toBigInt l + toBigInt r)

instance ringSignedInt :: Dividend8 m => Ring (SignedInt m) where
  sub l r = unsafeFromBigInt undefined (toBigInt l - toBigInt r)

-- | Signed n-bit integer: [−(2 `pow` n−1), (2 `pow` n−1))
mkSignedInt :: ∀ m. Dividend8 m => m -> BigInt -> Either Errors (SignedInt m)
mkSignedInt m = withBounds (lowerBound m) (upperBound m) where
  withBounds l u i
    | i < l = Left <<< mkErrors $ "SignedInt " <> show (toInt m) <> " can't hold a value "
                                <> I.toString i <> " < " <> I.toString l
    | i > u = Left <<< mkErrors $ "SignedInt " <> show (toInt m) <> " can't hold a value "
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
invert = toBigInt >>> I.not >>> unsafeFromBigInt undefined

complement :: ∀ a. Dividend8 a => SignedInt a -> SignedInt a
complement = invert >>> add one
