module Data.Ethereum.Abi.Type.Spec where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.ByteString (ByteString)
import Data.ByteString as B
import Data.Ethereum.Abi.Type (mkBytes)
import Data.Ethereum.Abi.Type.SignedInt.Spec as SignedInt
import Data.Ethereum.Abi.Type.UnsignedInt.Spec as UnsignedInt
import Data.Maybe (isJust, isNothing)
import Data.Typelevel.Num (d16, d8)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "Abi Types" do
    test "mkBytes D8"         $ quickCheck propBytes8
    test "mkBytes D16"        $ quickCheck propBytes16
  SignedInt.spec
  UnsignedInt.spec

propBytes8 :: ByteString -> Boolean
propBytes8 bs =
  let res = mkBytes d8 bs
  in if (B.length bs == 8) then isJust res else isNothing res

propBytes16 :: ByteString -> Boolean
propBytes16 bs =
  let res = mkBytes d16 bs
  in if (B.length bs == 16) then isJust res else isNothing res
