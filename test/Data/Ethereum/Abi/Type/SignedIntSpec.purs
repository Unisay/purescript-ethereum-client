module Data.Ethereum.Abi.Type.SignedInt.Spec where

import Prelude

import Arbitrary (ArbSignedInt64, ArbSignedInt8)
import Control.Monad.Eff.Random (RANDOM)
import Data.Ethereum.Abi.Class (enc)
import Data.Ethereum.Abi.Type.Property (propTypeEncIsDecodable, propTypeEncMultiple32b)
import Data.Newtype (unwrap)
import Property (isHexEncoding)
import Test.QuickCheck (Result)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "SignedInt" do
    test "encoded D8 produces a correct hex encoding" $
      quickCheck $ (unwrap >>> enc >>> isHexEncoding) :: ArbSignedInt8 -> Result
    test "encoded D64 produces a correct hex encoding" $
      quickCheck $ (unwrap >>> enc >>> isHexEncoding) :: ArbSignedInt64 -> Result
    test "encoded D8 is multiple 32" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbSignedInt8 -> Result
    test "encoded D64 is multiple 32" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbSignedInt64 -> Result
    test "encoded D8 is decodable" $
      quickCheck $ (unwrap >>> propTypeEncIsDecodable) :: ArbSignedInt8 -> Result
    test "encoded D64 is decodable" $
      quickCheck $ (unwrap >>> propTypeEncIsDecodable) :: ArbSignedInt64 -> Result
