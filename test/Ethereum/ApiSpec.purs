module Ethereum.Api.Spec where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject, jsonFalse)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.BigInt (BigInt, fromString)
import Data.Maybe (Maybe(..), fromJust)
import Ethereum.Api as E
import Ethereum.Rpc as Rpc
import Ethereum.Type (SyncStatus(..))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

spec :: ∀ e. TestSuite e
spec = do
  suite "Api" do
    test "web3_clientVersion" $ do
      let version = "test-client-version"
      actual <- E.run (respondWith version) E.web3ClientVersion
      Assert.equal version actual

    test "eth_syncing false" $ do
      actual <- E.run (respondWith jsonFalse) E.ethSyncing
      Assert.equal Nothing actual

    test "eth_syncing status" $ do
      let mockResponse = "currentBlock"  := "0xe40b5"
                      ~> "highestBlock"  := "0xe40c7"
                      ~> "startingBlock" := "0xe40b5"
                      ~> jsonEmptyObject

      actual <- E.run (respondWith mockResponse) E.ethSyncing
      let expected = Just $ SyncStatus { startingBlock : bigInt "0x0e40b5"
                                       , currentBlock  : bigInt "0x0e40c7"
                                       , highestBlock  : bigInt "0x0e40b5"
                                       }
      Assert.equal expected actual

newtype TestTransport = TestTransport Json

instance testTransport :: Rpc.Transport TestTransport e where
  call (TestTransport response) req = pure $ Rpc.Result response

respondWith :: ∀ r. EncodeJson r => r -> TestTransport
respondWith r = TestTransport $ encodeJson r

bigInt :: String -> BigInt
bigInt = unsafePartial fromJust <<< fromString
