module Ethereum.Api.Spec where

import Prelude

import Data.Argonaut.Core (Json, fromString, jsonEmptyObject, jsonFalse)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.BigInt (fromInt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing))
import Ethereum.Api (Address(..), Block(..), Tag(..), Wei(..))
import Ethereum.Api as E
import Ethereum.Rpc as Rpc
import Ethereum.Type (SyncStatus(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unsafe (unsafeByteString)

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
      let mockResponse = "startingBlock" := "0x1"
                      ~> "currentBlock"  := "0x2"
                      ~> "highestBlock"  := "0x3"
                      ~> jsonEmptyObject

      actual <- E.run (respondWith mockResponse) E.ethSyncing
      let expected = Just $ SyncStatus { startingBlock : Block $ fromInt 1
                                       , currentBlock  : Block $ fromInt 2
                                       , highestBlock  : Block $ fromInt 3
                                       }
      Assert.equal expected actual

    test "eth_blockNumber" $ do
      actual <- E.run (respondWith $ fromString "0x03") E.ethBlockNumber
      Assert.equal (Block $ fromInt 3) actual

    test "eth_getBalance" $ do
      let eth = E.ethGetBalance (Address $ unsafeByteString "00") (Right Latest)
      actual <- E.run (respondWith $ fromString "0x03") eth
      Assert.equal (Wei $ fromInt 3) actual

newtype TestTransport = TestTransport Json

instance testTransport :: Rpc.Transport TestTransport e where
  call (TestTransport response) req = pure $ Rpc.Result response

respondWith :: ∀ r. EncodeJson r => r -> TestTransport
respondWith r = TestTransport $ encodeJson r
