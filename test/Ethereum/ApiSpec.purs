module Ethereum.Api.Spec where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut.Core (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.BigInt (fromInt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Utils (unsafeRepeat)
import Ethereum.Api as E
import Ethereum.Hex (toHex)
import Network.Rpc.Json as Rpc
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unsafe (mkUnsafe)

spec :: ∀ e. TestSuite (console :: CONSOLE | e)
spec = do
  suite "Api" do
    let blockNumber = mkUnsafe 1
        blockHash = mkUnsafe $ unsafeRepeat 32 "32"
        txHash = mkUnsafe $ unsafeRepeat 32 "33"
        address = mkUnsafe $ unsafeRepeat 20 "20"
        latestBlock = Right E.Latest
        bytes = mkUnsafe $ unsafeRepeat 64 "64"
        quantity = mkUnsafe 42
        code = mkUnsafe $ unsafeRepeat 16 "16"
        signature = mkUnsafe $ unsafeRepeat 32 "32"

    test "web3_clientVersion" do
      let version = "test-client-version"
          rq = Rpc.Request {id: 1, method: "web3_clientVersion", params: []}
          rp = Rpc.Response 1 $ pure version
      actual <- E.run (Match rq rp) E.web3ClientVersion
      version `equal` actual

    test "eth_syncing false" do
      let rq = Rpc.Request {id: 1, method: "eth_syncing", params: []}
          rp = Rpc.Response 1 (Right false)
      actual <- E.run (Match rq rp) E.ethSyncing
      Nothing `equal` actual

    test "eth_syncing status" do
      let syncStatus = { startingBlock : mkUnsafe 1
                       , currentBlock  : mkUnsafe 2
                       , highestBlock  : mkUnsafe 3
                       }
          syncStatusJson = "startingBlock" := syncStatus.startingBlock
                        ~> "currentBlock"  := syncStatus.currentBlock
                        ~> "highestBlock"  := syncStatus.highestBlock
                        ~> jsonEmptyObject
          rq = Rpc.Request {id: 1, method: "eth_syncing", params: []}
          rp = Rpc.Response 1 (Right syncStatusJson)
      actual <- E.run (Match rq rp) E.ethSyncing
      Just (E.SyncStatus syncStatus) `equal` actual

    test "eth_blockNumber" $ do
      let rq = Rpc.Request {id: 1, method: "eth_blockNumber", params: []}
          rp = Rpc.Response 1 $ pure blockNumber
      actual <- E.run (Match rq rp) E.ethBlockNumber
      blockNumber `equal` actual

    test "eth_getBalance" do
      let wei = E.Wei $ fromInt 3
          rq = Rpc.Request { id: 1
                           , method: "eth_getBalance"
                           , params: [ toHex address
                                     , "latest"
                                     ]
                           }
          rp = Rpc.Response 1 $ pure wei
          eth = E.ethGetBalance address latestBlock
      actual <- E.run (Match rq rp) eth
      wei `equal` actual

    test "eth_getStorageAt" do
      let rq = Rpc.Request { id: 1
                           , method: "eth_getStorageAt"
                           , params: [ toHex address
                                     , toHex 42
                                     , "latest"
                                     ]
                           }
          rp = Rpc.Response 1 $ pure bytes
          eth = E.ethGetStorageAt address 42 latestBlock
      actual <- E.run (Match rq rp) eth
      bytes `equal` actual

    test "eth_getTransactionCount" do
      let eth = E.ethGetTransactionCount address latestBlock
          rq = Rpc.Request { id: 1
                           , method: "eth_getTransactionCount"
                           , params: [toHex address, "latest"]
                           }
          rp = Rpc.Response 1 $ pure (Just quantity)
      actual <- E.run (Match rq rp) eth
      Just quantity `equal` actual

    test "eth_getBlockTransactionCountByHash" do
      let eth = E.ethGetBlockTransactionCountByHash blockHash
          rq = Rpc.Request { id: 1
                           , method: "eth_getBlockTransactionCountByHash"
                           , params: [toHex blockHash]
                           }
          rp = Rpc.Response 1 $ pure (Just quantity)
      actual <- E.run (Match rq rp) eth
      Just quantity `equal` actual

    test "eth_getBlockTransactionCountByNumber" do
      let eth = E.ethGetBlockTransactionCountByNumber latestBlock
          rq = Rpc.Request { id: 1
                           , method: "eth_getBlockTransactionCountByNumber"
                           , params: ["latest"]
                           }
          rp = Rpc.Response 1 $ pure (Just quantity)
      actual <- E.run (Match rq rp) eth
      Just quantity `equal` actual

    test "eth_getUncleCountByBlockHash" do
      let eth = E.ethGetUncleCountByBlockHash blockHash
          rq = Rpc.Request { id: 1
                           , method: "eth_getUncleCountByBlockHash"
                           , params: [toHex blockHash]
                           }
          rp = Rpc.Response 1 $ pure (Just quantity)
      actual <- E.run (Match rq rp) eth
      Just quantity `equal` actual

    test "eth_getUncleCountByBlockNumber" do
      let eth = E.ethGetUncleCountByBlockNumber latestBlock
          rq = Rpc.Request { id: 1
                           , method: "eth_getUncleCountByBlockNumber"
                           , params: ["latest"]
                           }
          rp = Rpc.Response 1 $ pure (Just quantity)
      actual <- E.run (Match rq rp) eth
      Just quantity `equal` actual

    test "eth_getCode" do
      let eth = E.ethGetCode address latestBlock
          rq = Rpc.Request { id: 1
                           , method: "eth_getCode"
                           , params: [toHex address, "latest"]
                           }
          rp = Rpc.Response 1 $ pure code
      actual <- E.run (Match rq rp) eth
      code `equal` actual

    test "eth_sign" do
      let eth = E.ethSign address bytes
          rq = Rpc.Request { id: 1
                           , method: "eth_sign"
                           , params: [ toHex address
                                     , toHex bytes
                                     ]
                           }
          rp = Rpc.Response 1 $ pure signature
      actual <- E.run (Match rq rp) eth
      signature `equal` actual

    let transaction = E.Transaction { from: address
                                    , to: Just address
                                    , gas: Just $ mkUnsafe 600000
                                    , gasPrice: Nothing
                                    , value: Nothing
                                    , data: Left $ E.Code (mkUnsafe "00")
                                    , nonce: Just $ mkUnsafe 42
                                    }

    test "eth_sendTransaction" do
      let eth = E.ethSendTransaction transaction
          rq = Rpc.Request { id: 1
                           , method: "eth_sendTransaction"
                           , params: [stringify $ encodeJson transaction]
                           }
          rp = Rpc.Response 1 $ Right $ Just txHash
      actual <- E.run (Match rq rp) eth
      Just txHash `equal` actual

    test "eth_sendRawTransaction" do
      let eth = E.ethSendRawTransaction bytes
          rq = Rpc.Request { id: 1
                           , method: "eth_sendRawTransaction"
                           , params: [toHex bytes]
                           }
          rp = Rpc.Response 1 $ Right $ Just txHash
      actual <- E.run (Match rq rp) eth
      Just txHash `equal` actual

    test "eth_call" do
      let eth = E.ethCall transaction latestBlock
          rq = Rpc.Request { id: 1
                           , method: "eth_call"
                           , params: [ stringify $ encodeJson transaction
                                     , "latest"
                                     ]
                           }
          rp = Rpc.Response 1 $ Right $ bytes
      actual <- E.run (Match rq rp) eth
      bytes `equal` actual



    -- TODO: Sign and Verify test scenario https://gist.github.com/bas-vk/d46d83da2b2b4721efb0907aecdb7ebd

data Match a = Match Rpc.Request (Rpc.Response a)

instance matchTransport :: EncodeJson a =>
                           Rpc.Transport (Match a) (console :: CONSOLE | e) where
  call :: Match a -> Rpc.Request -> Aff (console :: CONSOLE | e) (Rpc.Response Json)
  call (Match expectedRequest response) actualRequest = do
    resp <- if (expectedRequest == actualRequest)
            then pure $ encodeJson <$> response
            else pure $ Rpc.Response 1 (Left $ Rpc.Error
              { code: 1
              , message: "Failed to match expected request ("
                        <> stringify (encodeJson expectedRequest)
                        <> ") with the actual request ("
                        <> stringify (encodeJson actualRequest)
                        <> ")"
              , data: Nothing
              })
    -- log $ "Received: " <> (stringify $ encodeJson actualRequest)
    -- log $ "Replied: " <> (stringify $ encodeJson resp)
    pure resp
