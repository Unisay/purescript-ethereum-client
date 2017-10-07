module Data.Ethereum.Abi.Type.UnsignedInt.Spec where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.BigInt (BigInt, fromInt)
import Data.Either (isLeft, isRight)
import Data.Ethereum.Abi.Type (UnsignedInt, mkUnsignedInt)
import Data.Ethereum.Abi.Type.Property (propDecodableEnc, propTypeEncMultiple32b)
import Data.Typelevel.Num (D8, D16, d16, d8)
import Test.QuickCheck (class Arbitrary, Result, arbitrary)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "UnsignedInt" do
    test "mkUnsignedInt D8"   $ quickCheck propUnsignedInt8
    test "mkUnsignedInt D16"  $ quickCheck propUnsignedInt16
    test "enc UnsignedInt 8"  $ quickCheck propUnsignedIntEnc8
    test "enc UnsignedInt 16" $ quickCheck propUnsignedIntEnc16
    test "enc UnsignedInt 8 is decodable"  $ quickCheck propDecodableEnc8
    test "enc UnsignedInt 16 is decodable" $ quickCheck propDecodableEnc16

newtype ArbBigInt = ArbBigInt BigInt

instance arbitraryBigInt :: Arbitrary ArbBigInt where
  arbitrary = ArbBigInt <<< fromInt <$> arbitrary

propUnsignedInt8 :: ArbBigInt -> Boolean
propUnsignedInt8 (ArbBigInt i) =
  let v = mkUnsignedInt d8 i in if (zero <= i && i < fromInt 256) then isRight v else isLeft v

propUnsignedInt16 :: ArbBigInt -> Boolean
propUnsignedInt16 (ArbBigInt i) =
  let v = mkUnsignedInt d16 i in if (zero <= i && i < fromInt 65536) then isRight v else isLeft v

propUnsignedIntEnc8 :: UnsignedInt D8 -> Result
propUnsignedIntEnc8 = propTypeEncMultiple32b

propUnsignedIntEnc16 :: UnsignedInt D16 -> Result
propUnsignedIntEnc16 = propTypeEncMultiple32b

propDecodableEnc8 :: UnsignedInt D8 -> Result
propDecodableEnc8 = propDecodableEnc

propDecodableEnc16 :: UnsignedInt D16 -> Result
propDecodableEnc16 = propDecodableEnc
