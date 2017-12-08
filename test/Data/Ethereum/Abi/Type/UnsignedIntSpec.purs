module Data.Ethereum.Abi.Type.UnsignedInt.Spec where

import Prelude

import Arbitrary (ArbUnsignedInt64, ArbUnsignedInt8)
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
  suite "UnsignedInt" do
    test "encoded D8 produces a correct hex encoding" $
      quickCheck $ (unwrap >>> enc >>> isHexEncoding) :: ArbUnsignedInt8 -> Result
    test "encoded D64 produces a correct hex encoding" $
      quickCheck $ (unwrap >>> enc >>> isHexEncoding) :: ArbUnsignedInt64 -> Result
    test "encoded D8 is multiple 32" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbUnsignedInt8 -> Result
    test "encoded D64 is multiple 32" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbUnsignedInt64 -> Result
    test "encoded D8 is decodable" $
      quickCheck $ (unwrap >>> propTypeEncIsDecodable) :: ArbUnsignedInt8 -> Result
    test "encoded D64 is decodable" $
      quickCheck $ (unwrap >>> propTypeEncIsDecodable) :: ArbUnsignedInt64 -> Result
