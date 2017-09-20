module Ethereum.Text.Spec where

import Prelude
import Data.ByteString as BS
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Ethereum.Text (toHex, fromHex, fromHexQuantity)
import Node.Encoding (Encoding(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

spec :: ∀ e. TestSuite e
spec = do
  suite "Text" do
    test "toHex" $ "0x02cafebabe" `equal` toHex (BS.fromString "02cafebabe" Hex)
    test "fromHex" $ (BS.fromString "02cafebabe" Hex) `equal` fromHex "0x02cafebabe"
    test "fromHexQuantity" $ Just [1024, 1024, 0] `equal` traverse fromHexQuantity ["0x400", "0x0400", "0x0"]
