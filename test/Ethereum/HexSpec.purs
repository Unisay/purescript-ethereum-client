module Ethereum.Hex.Spec where

import Prelude
import Data.BigInt as I
import Data.ByteString as BS
import Arbitrary (ArbAddress, ArbBigInt)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (drop)
import Data.ByteString (ByteString)
import Data.Char.Unicode (isHexDigit)
import Data.Either (Either(Right))
import Data.Ethereum (Address)
import Data.Foldable (all)
import Data.String (toCharArray)
import Data.String.Utils (startsWith, unsafeRepeat)
import Data.Traversable (traverse)
import Ethereum (class ToHex)
import Ethereum.Hex (fromHex, toHex)
import Test.MkUnsafe (mkUnsafe)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "To Hex" do
    let byteString :: ByteString
        byteString = mkUnsafe "02cafebabe"
        bigInt = I.fromInt 934069
    test "toHex ByteString" $
      "0x02cafebabe" `equal` toHex byteString
    test "toHex ByteString has 0x prefix" $
      quickCheck (propEncodingHas0xPrefix :: ByteString -> Boolean)
    test "toHex ByteString has only hex digits" $
      quickCheck (propHasOnlyHexDigits :: ByteString -> Boolean)

    test "toHex BigInt" $
      "0xe40b5" `equal` toHex bigInt
    test "toHex BigInt has 0x prefix" $
      quickCheck (propEncodingHas0xPrefix :: ArbBigInt -> Boolean)
    test "toHex BigInt has only hex digits" $
      quickCheck (propHasOnlyHexDigits :: ArbBigInt -> Boolean)

    test "toHex Address" do
      let addressStr = unsafeRepeat 20 "20"
          address = mkUnsafe addressStr :: Address
      ("0x" <> addressStr) `equal` toHex address
    test "toHex Address has 0x prefix" $
      quickCheck (propEncodingHas0xPrefix :: ArbAddress -> Boolean)
    test "toHex Address has only hex digits" $
      quickCheck (propHasOnlyHexDigits :: ArbAddress -> Boolean)

  suite "From Hex" do
    test "fromHex ByteString" do
      let expected = Right [mkUnsafe "02cafebabe" :: BS.ByteString, mkUnsafe "00", mkUnsafe "0e40b5"]
      expected `equal` traverse fromHex ["0x02cafebabe", "0x0", "e40b5"]
    test "fromHex BigInt" do
      let expected = Right [I.fromInt 1024, I.fromInt 1024, I.fromInt 0, I.fromInt 934069]
      expected `equal` traverse fromHex ["0x400", "0x0400", "0x0", "0xe40b5"]
    test "fromHex Int" do
      let expected = Right [1024, 1024, 0, 934069]
      expected `equal` traverse fromHex ["0x400", "0x0400", "0x0", "0xe40b5"]



propHasOnlyHexDigits :: ∀ a. ToHex a => a -> Boolean
propHasOnlyHexDigits = toHex >>> toCharArray >>> drop 2 >>> all isHexDigit

propEncodingHas0xPrefix :: ∀ a. ToHex a => a -> Boolean
propEncodingHas0xPrefix = toHex >>> startsWith "0x"
