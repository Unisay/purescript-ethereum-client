module Ethereum.Hex.Spec where

import Prelude

import Data.BigInt as I
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Either (Either(Right))
import Data.Traversable (traverse)
import Ethereum.Text (fromHex, toHex)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unsafe (mkUnsafe)

spec :: ∀ e. TestSuite e
spec = do
  suite "Hex" do
    let byteString :: ByteString
        byteString = mkUnsafe "02cafebabe"
        bigInt = I.fromInt 934069
    test "toHex ByteString" $
      "0x02cafebabe" `equal` toHex byteString
    test "toHex BigInt" $
      "0xe40b5" `equal` toHex bigInt
    test "fromHex ByteString" $
      let expected = Right [mkUnsafe "02cafebabe" :: BS.ByteString, mkUnsafe "00", mkUnsafe "0e40b5"]
      in expected `equal` traverse fromHex ["0x02cafebabe", "0x0", "e40b5"]
    test "fromHex BigInt" $
      let expected = Right [I.fromInt 1024, I.fromInt 1024, I.fromInt 0, I.fromInt 934069]
      in expected `equal` traverse fromHex ["0x400", "0x0400", "0x0", "0xe40b5"]
    test "fromHex Int" $
      let expected = Right [1024, 1024, 0, 934069]
      in expected `equal` traverse fromHex ["0x400", "0x0400", "0x0", "0xe40b5"]
