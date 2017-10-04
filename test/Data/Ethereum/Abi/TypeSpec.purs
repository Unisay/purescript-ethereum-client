module Data.Ethereum.Abi.Type.Spec where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.BigInt (BigInt, fromInt)
import Data.ByteString (ByteString)
import Data.ByteString as B
import Data.Ethereum.Abi.Type (mkUnsignedInt, mkSignedInt, mkBytes)
import Data.Maybe (isJust, isNothing)
import Data.Typelevel.Num (d16, d8)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "Abi Types" do
    test "mkUnsignedInt D8"  $ quickCheck propUnsignedInt8
    test "mkUnsignedInt D16" $ quickCheck propUnsignedInt16
    test "mkSignedInt D8"    $ quickCheck propSignedInt8
    test "mkSignedInt D16"   $ quickCheck propSignedInt16
    test "mkBytes D8"        $ quickCheck propBytes8
    test "mkBytes D16"       $ quickCheck propBytes16
    test "mkBool"            $ quickCheck propBool

newtype ArbBigInt = ArbBigInt BigInt

instance arbitraryBigInt :: Arbitrary ArbBigInt where
  arbitrary = ArbBigInt <<< fromInt <$> arbitrary

propUnsignedInt8 :: ArbBigInt -> Boolean
propUnsignedInt8 (ArbBigInt i) =
  let v = mkUnsignedInt d8 i in if (zero <= i && i < fromInt 256) then isJust v else isNothing v

propUnsignedInt16 :: ArbBigInt -> Boolean
propUnsignedInt16 (ArbBigInt i) =
  let v = mkUnsignedInt d16 i in if (zero <= i && i < fromInt 65536) then isJust v else isNothing v

propSignedInt8 :: ArbBigInt -> Boolean
propSignedInt8 (ArbBigInt i) =
  let v = mkSignedInt d8 i in if (-(fromInt 128) <= i && i < fromInt 128) then isJust v else isNothing v

propSignedInt16 :: ArbBigInt -> Boolean
propSignedInt16 (ArbBigInt i) =
  let v = mkSignedInt d16 i in if (-(fromInt 32768) <= i && i < fromInt 32768) then isJust v else isNothing v

propBytes8 :: ByteString -> Boolean
propBytes8 bs =
  let res = mkBytes d8 bs in if (B.length bs == 8) then isJust res else isNothing res

propBytes16 :: ByteString -> Boolean
propBytes16 bs =
  let res = mkBytes d16 bs in if (B.length bs == 16) then isJust res else isNothing res

propBool :: Boolean -> Boolean
propBool b = true -- TODO
