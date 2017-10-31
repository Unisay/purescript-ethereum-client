module Data.Ethereum.Abi.Type.UnsignedInt.Spec where

import Prelude

import Arbitrary (ArbUnsignedInt64, ArbUnsignedInt8)
import Control.Monad.Eff.Random (RANDOM)
import Data.Ethereum.Abi.Type.Property (propDecodableEnc, propTypeEncMultiple32b)
import Data.Newtype (unwrap)
import Test.QuickCheck (Result)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "UnsignedInt" do
    test "enc UnsignedInt 8"  $ quickCheck propUnsignedIntEnc8
    test "enc UnsignedInt 64" $ quickCheck propUnsignedIntEnc64
    test "enc UnsignedInt 8 is decodable"  $ quickCheck propDecodableEnc8
    test "enc UnsignedInt 64 is decodable" $ quickCheck propDecodableEnc64

propUnsignedIntEnc8 :: ArbUnsignedInt8 -> Result
propUnsignedIntEnc8 = unwrap >>> propTypeEncMultiple32b

propUnsignedIntEnc64 :: ArbUnsignedInt64 -> Result
propUnsignedIntEnc64 = unwrap >>> propTypeEncMultiple32b

propDecodableEnc8 :: ArbUnsignedInt8 -> Result
propDecodableEnc8 = unwrap >>> propDecodableEnc

propDecodableEnc64 :: ArbUnsignedInt64 -> Result
propDecodableEnc64 = unwrap >>> propDecodableEnc
