module Ethereum.Text.Spec where

import Prelude
import Data.BigInt (fromInt)
import Data.ByteString as BS
import Data.Maybe (Maybe(Just))
import Data.Traversable (traverse)
import Ethereum.Text (fromHex, fromHexQuantity, fromHexQuantity', toHex)
import Node.Encoding (Encoding(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unsafe (unsafeByteString)

spec :: ∀ e. TestSuite e
spec = do
  suite "Text" do
    test "toHex" $
      "0x02cafebabe" `equal` toHex (unsafeByteString "02cafebabe")
    test "fromHex" $
      let expected = (\s -> BS.fromString s Hex) <$> ["02cafebabe", "00", "0e40b5"]
      in expected `equal` (fromHex <$> ["0x02cafebabe", "0x0", "e40b5"])
    test "fromHexQuantity" $
      let expected = Just [fromInt 1024, fromInt 1024, fromInt 0, fromInt 934069]
      in expected `equal` traverse fromHexQuantity ["0x400", "0x0400", "0x0", "0xe40b5"]
    test "fromHexQuantity'" $
      let expected = Just [1024, 1024, 0, 934069]
      in expected `equal` traverse fromHexQuantity' ["0x400", "0x0400", "0x0", "0xe40b5"]
