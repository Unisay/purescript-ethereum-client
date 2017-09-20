module Ethereum.Api.Spec where

import Prelude
import Data.ByteString as BS
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Ethereum.Text (toHex, fromHex, fromHexQuantity)
import Node.Encoding (Encoding(..))
import Test.Unit.Assert (equal)
import Test.Unit (TestSuite, failure, suite, test)

spec :: ∀ e. TestSuite e
spec = do
  suite "Api" do
    test "parseSyncStatus" $ failure "test"
