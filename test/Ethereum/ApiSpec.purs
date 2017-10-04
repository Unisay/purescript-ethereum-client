module Ethereum.Api.Spec where

import Prelude

import Data.Argonaut.Core (Json, fromString, jsonEmptyObject, jsonFalse, jsonNull)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.BigInt (fromInt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing, Just))
import Ethereum.Api as E
import Ethereum.Type (SyncStatus(..))
import Network.Rpc.Json as Rpc
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unsafe (mkUnsafe)

spec :: ∀ e. TestSuite e
spec = do
  suite "Api" do
    let blockNumber = mkUnsafe 1
        blockHash = mkUnsafe "b903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238"
        address = mkUnsafe "0102030405060708091011121314151617181920"

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
      let expected = Just $ SyncStatus { startingBlock : mkUnsafe 1
                                       , currentBlock  : mkUnsafe 2
                                       , highestBlock  : mkUnsafe 3
                                       }
      Assert.equal expected actual

    test "eth_blockNumber" $ do
      actual <- E.run (respondWith $ fromString "0x01") E.ethBlockNumber
      Assert.equal blockNumber actual

    test "eth_getBalance" do
      let eth = E.ethGetBalance address (Right E.Latest)
      actual <- E.run (respondWith $ fromString "0x03") eth
      Assert.equal (E.Wei $ fromInt 3) actual

    test "eth_getStorageAt" do
      let eth = E.ethGetStorageAt address 42 (Right E.Earliest)
      actual <- E.run (respondWith $ fromString "0x010203") eth
      Assert.equal (mkUnsafe "010203") actual

    test "eth_getTransactionCount" do
      let eth = E.ethGetTransactionCount address (Left blockNumber)
      actual <- E.run (respondWith $ fromString "0x09") eth
      Assert.equal (Just $ mkUnsafe 9) actual

    test "eth_getTransactionCount null" do
      let eth = E.ethGetTransactionCount address (Left blockNumber)
      actual <- E.run (respondWith jsonNull) eth
      Assert.equal Nothing actual

    test "eth_getBlockTransactionCountByHash" do
      let eth = E.ethGetBlockTransactionCountByHash blockHash
      actual <- E.run (respondWith $ fromString "0x08") eth
      Assert.equal (Just $ mkUnsafe 8) actual

    test "eth_getBlockTransactionCountByHash null" do
      let eth = E.ethGetBlockTransactionCountByHash blockHash
      actual <- E.run (respondWith jsonNull) eth
      Assert.equal Nothing actual

    test "eth_getBlockTransactionCountByNumber" do
      let eth = E.ethGetBlockTransactionCountByNumber (Right E.Latest)
      actual <- E.run (respondWith $ fromString "0x08") eth
      Assert.equal (Just $ mkUnsafe 8) actual

    test "eth_getBlockTransactionCountByNumber null" do
      let eth = E.ethGetBlockTransactionCountByNumber (Right E.Earliest)
      actual <- E.run (respondWith jsonNull) eth
      Assert.equal Nothing actual

    test "eth_getUncleCountByBlockHash" do
      let eth = E.ethGetUncleCountByBlockHash blockHash
      actual <- E.run (respondWith $ fromString "0x08") eth
      Assert.equal (Just $ mkUnsafe 8) actual

    test "eth_getUncleCountByBlockHash null" do
      let eth = E.ethGetUncleCountByBlockHash blockHash
      actual <- E.run (respondWith jsonNull) eth
      Assert.equal Nothing actual

    test "eth_getUncleCountByBlockNumber" do
      let eth = E.ethGetUncleCountByBlockNumber (Right E.Latest)
      actual <- E.run (respondWith $ fromString "0x08") eth
      Assert.equal (Just $ mkUnsafe 8) actual

    test "eth_getUncleCountByBlockNumber null" do
      let eth = E.ethGetUncleCountByBlockNumber (Right E.Latest)
      actual <- E.run (respondWith jsonNull) eth
      Assert.equal Nothing actual

    test "eth_getCode" do
      let eth = E.ethGetCode address (Right E.Latest)
      actual <- E.run (respondWith $ fromString "0x010203") eth
      Assert.equal (mkUnsafe "010203") actual

    test "eth_sign" do
      let eth = E.ethSign address (mkUnsafe "123456")
      actual <- E.run (respondWith $ fromString "0x010203") eth
      Assert.equal (mkUnsafe "010203") actual

    -- TODO: Sign and Verify test scenario https://gist.github.com/bas-vk/d46d83da2b2b4721efb0907aecdb7ebd


newtype TestTransport = TestTransport Json

instance testTransport :: Rpc.Transport TestTransport e where
  call (TestTransport response) req = pure $ Rpc.Response (pure response)

respondWith :: ∀ r. EncodeJson r => r -> TestTransport
respondWith r = TestTransport $ encodeJson r
