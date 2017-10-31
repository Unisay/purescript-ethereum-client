module Data.Ethereum.Abi.Type.SignedInt.Spec where

import Prelude
import Arbitrary (ArbSignedInt64, ArbSignedInt8)
import Control.Monad.Eff.Random (RANDOM)
import Data.BigInt (BigInt, fromInt)
import Data.Either (isLeft, isRight)
import Data.Ethereum.Abi.Class (enc)
import Data.Ethereum.Abi.Type.Class (class Dividend8)
import Data.Ethereum.Abi.Type.Property (propDecodableEnc, propTypeEncMultiple32b)
import Data.Newtype (unwrap)
import Data.Typelevel.Num (d16, d8)
import Property (isHex, (<&&>))
import Test.QuickCheck (class Arbitrary, Result, arbitrary, (/==), (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "SignedInt" do
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
