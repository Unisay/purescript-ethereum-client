module Data.Ethereum.Abi.Type.Spec where

import Prelude
import Data.ByteString as B
import Control.Monad.Eff.Random (RANDOM)
import Data.ByteString (ByteString)
import Data.Ethereum.Abi.Type (mkUnsignedInt, mkSignedInt, mkBytes)
import Data.Maybe (isJust, isNothing)
import Data.Typelevel.Num (d16, d8)
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


propUnsignedInt8 :: Int -> Boolean
propUnsignedInt8 i =
  let v = mkUnsignedInt d8 i in if (0 <= i && i < 256) then isJust v else isNothing v

propUnsignedInt16 :: Int -> Boolean
propUnsignedInt16 i =
  let v = mkUnsignedInt d16 i in if (0 <= i && i < 65536) then isJust v else isNothing v

propSignedInt8 :: Int -> Boolean
propSignedInt8 i =
  let v = mkSignedInt d8 i in if (-128 <= i && i < 128) then isJust v else isNothing v

propSignedInt16 :: Int -> Boolean
propSignedInt16 i =
  let v = mkSignedInt d16 i in if (-32768 <= i && i < 32768) then isJust v else isNothing v

propBytes8 :: Int -> ByteString -> Boolean
propBytes8 b bs =
  let res = mkBytes d8 bs in if (B.length bs == 8) then isJust res else isNothing res

propBytes16 :: Int -> ByteString -> Boolean
propBytes16 b bs =
  let res = mkBytes d16 bs in if (B.length bs == 16) then isJust res else isNothing res

propBool :: Boolean -> Boolean
propBool b = true -- TODO
