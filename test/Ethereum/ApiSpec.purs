module Ethereum.Api.Spec where

import Prelude

import Data.Argonaut.Core (Json, fromString, jsonEmptyObject, jsonFalse, jsonNull)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.BigInt (fromInt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Ethereum.Api (Address(..), BlockHash(..), BlockNumber(..), Quantity(..), Tag(..), Wei(..))
import Ethereum.Api as E
import Network.Rpc.Json as Rpc
import Ethereum.Text (fromHex)
import Ethereum.Type (SyncStatus(..))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unsafe (unsafeByteString)

spec :: ∀ e. TestSuite e
spec = do
  suite "Api" do
    let blockNumber = BlockNumber $ fromInt 1
        blockHash = BlockHash $ unsafePartial $ fromJust $ fromHex "b903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238"
        address = Address $ unsafeByteString "08"

    test "web3_clientVersion" do
      let version = "test-client-version"
      actual <- E.run (respondWith version) E.web3ClientVersion
      Assert.equal version actual

    test "eth_syncing false" do
      actual <- E.run (respondWith jsonFalse) E.ethSyncing
      Assert.equal Nothing actual

    test "eth_syncing status" do
      let mockResponse = "startingBlock" := "0x1"
                      ~> "currentBlock"  := "0x2"
                      ~> "highestBlock"  := "0x3"
                      ~> jsonEmptyObject

      actual <- E.run (respondWith mockResponse) E.ethSyncing
      let expected = Just $ SyncStatus { startingBlock : BlockNumber $ fromInt 1
                                       , currentBlock  : BlockNumber $ fromInt 2
                                       , highestBlock  : BlockNumber $ fromInt 3
                                       }
      Assert.equal expected actual

    test "eth_blockNumber" $ do
      actual <- E.run (respondWith $ fromString "0x01") E.ethBlockNumber
      Assert.equal blockNumber actual

    test "eth_getBalance" do
      let eth = E.ethGetBalance address (Right Latest)
      actual <- E.run (respondWith $ fromString "0x03") eth
      Assert.equal (Wei $ fromInt 3) actual

    test "eth_getStorageAt" do
      let eth = E.ethGetStorageAt address 42 (Right Earliest)
      actual <- E.run (respondWith $ fromString "0x010203") eth
      Assert.equal (unsafeByteString "010203") actual

    test "eth_getTransactionCount" do
      let eth = E.ethGetTransactionCount address (Left blockNumber)
      actual <- E.run (respondWith $ fromString "0x09") eth
      Assert.equal (Quantity 9) actual

    test "eth_getBlockTransactionCountByHash just" do
      let eth = E.ethGetBlockTransactionCountByHash blockHash
      actual <- E.run (respondWith $ fromString "0x08") eth
      Assert.equal (Just $ Quantity 8) actual

    test "eth_getBlockTransactionCountByHash nothing" do
      let eth = E.ethGetBlockTransactionCountByHash blockHash
      actual <- E.run (respondWith jsonNull) eth
      Assert.equal Nothing actual

    test "eth_getUncleCountByBlockNumber" do
      let eth = E.ethGetUncleCountByBlockNumber (Right Latest)
      actual <- E.run (respondWith $ fromString "0x08") eth
      Assert.equal (Quantity 8) actual

newtype TestTransport = TestTransport Json

instance testTransport :: Rpc.Transport TestTransport e where
  call (TestTransport response) req = pure $ Rpc.Response (pure response)

respondWith :: ∀ r. EncodeJson r => r -> TestTransport
respondWith r = TestTransport $ encodeJson r
