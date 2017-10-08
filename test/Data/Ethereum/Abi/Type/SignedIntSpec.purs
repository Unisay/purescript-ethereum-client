module Data.Ethereum.Abi.Type.SignedInt.Spec where

import Prelude

import Arbitrary (ArbSignedInt64, ArbSignedInt8)
import Control.Monad.Eff.Random (RANDOM)
import Data.BigInt (BigInt, fromInt)
import Data.Either (isLeft, isRight)
import Data.Ethereum.Abi.Class (enc)
import Data.Ethereum.Abi.Type.Class (class Dividend8)
import Data.Ethereum.Abi.Type.Property (propDecodableEnc, propTypeEncMultiple32b)
import Data.Ethereum.Abi.Type.SignedInt (SignedInt, complement, invert, isNegative, mkSignedInt)
import Data.Newtype (unwrap)
import Data.Typelevel.Num (d16, d8)
import Property (isHex, (<&&>))
import Test.QuickCheck (class Arbitrary, Result, arbitrary, (/==), (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "SignedInt" do
    test "mkSignedInt 8" $
      quickCheck propSignedInt8
    test "mkSignedInt 16" $
      quickCheck propSignedInt16
    test "invert SignedInt 8" $
      quickCheck ((unwrap >>> propInvert) :: ArbSignedInt8 -> Result)
    test "invert SignedInt 64" $
      quickCheck ((unwrap >>> propInvert) :: ArbSignedInt64 -> Result)
    test "complement SignedInt 8" $
      quickCheck ((unwrap >>> propComplement) :: ArbSignedInt8 -> Result)
    test "complement SignedInt 64" $
      quickCheck ((unwrap >>> propComplement) :: ArbSignedInt64 -> Result)
    test "enc SignedInt 8 produces a correct hex encoding" $
      quickCheck ((unwrap >>> enc >>> isHex) :: ArbSignedInt8 -> Result)
    test "enc SignedInt 64 produces a correct hex encoding" $
      quickCheck ((unwrap >>> enc >>> isHex) :: ArbSignedInt64 -> Result)
    test "enc SignedInt 8 is multiple 32" $
      quickCheck ((unwrap >>> propTypeEncMultiple32b) :: ArbSignedInt8 -> Result)
    test "enc SignedInt 64 is multiple 32" $
      quickCheck ((unwrap >>> propTypeEncMultiple32b) :: ArbSignedInt64 -> Result)
    test "enc SignedInt 8 is decodable" $
      quickCheck ((unwrap >>> propDecodableEnc) :: ArbSignedInt8 -> Result)
    test "enc SignedInt 64 is decodable" $
      quickCheck ((unwrap >>> propDecodableEnc) :: ArbSignedInt64 -> Result)

newtype ArbBigInt = ArbBigInt BigInt

instance arbitraryBigInt :: Arbitrary ArbBigInt where
  arbitrary = ArbBigInt <<< fromInt <$> arbitrary

propSignedInt8 :: ArbBigInt -> Boolean
propSignedInt8 (ArbBigInt i) =
  let v = mkSignedInt d8 i
  in if (fromInt (-128) <= i && i < fromInt 128)
     then isRight v
     else isLeft v

propSignedInt16 :: ArbBigInt -> Boolean
propSignedInt16 (ArbBigInt i) =
  let v = mkSignedInt d16 i
  in if (fromInt (-32768) <= i && i < fromInt 32768)
     then isRight v
     else isLeft v

propInvert :: ∀ a. Dividend8 a => SignedInt a -> Result
propInvert original =
  let inverted = invert original
      signConsistency = (isNegative original == isNegative inverted) <?> "inverted SignedInt changed sign"
      doubleInversion = original === invert inverted
  in signConsistency <&&> doubleInversion

propComplement :: ∀ a. Dividend8 a => SignedInt a -> Result
propComplement original =
  let complemented = complement original
      sum = original + complemented
      propSum = zero == sum <?> "The sum of a number and its two's complement isn't equal 0"
      equality = complemented /== original
  in propSum <&&> equality
