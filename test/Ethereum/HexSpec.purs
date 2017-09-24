module Ethereum.Hex.Spec where

import Prelude
import Data.BigInt as BI
import Data.ByteString as BS
import Data.Maybe (Maybe(Just))
import Data.Traversable (traverse)
import Ethereum.Text (fromHex, toHex)
import Node.Encoding (Encoding(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unsafe (unsafeByteString)

spec :: ∀ e. TestSuite e
spec = do
  suite "Hex" do
    test "toHex ByteString" $ "0x02cafebabe" `equal` toHex (unsafeByteString "02cafebabe")
    test "toHex BigInt" $ "0xe40b5" `equal` toHex (BI.fromInt 934069)
    test "fromHex ByteString" $
      let expected = (\s -> BS.fromString s Hex) <$> ["02cafebabe", "00", "0e40b5"]
      in expected `equal` (fromHex <$> ["0x02cafebabe", "0x0", "e40b5"])
    test "fromHex BigInt" $
      let expected = Just [BI.fromInt 1024, BI.fromInt 1024, BI.fromInt 0, BI.fromInt 934069]
      in expected `equal` traverse fromHex ["0x400", "0x0400", "0x0", "0xe40b5"]
    test "fromHex Int" $
      let expected = Just [1024, 1024, 0, 934069]
      in expected `equal` traverse fromHex ["0x400", "0x0400", "0x0", "0xe40b5"]
