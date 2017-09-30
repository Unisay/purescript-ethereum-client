module Ethereum.Api
  ( EthF
  , Eth
  , module E
  , run
  , netVersion
  , netListening
  , netPeerCount
  , web3ClientVersion
  , keccak256
  , ethProtocolVersion
  , ethSyncing
  , ethCoinbase
  , ethMining
  , ethHashrate
  , ethGasPrice
  , ethAccounts
  , ethBlockNumber
  , ethGetBalance
  , ethGetStorageAt
  , ethGetTransactionCount
  , ethGetBlockTransactionCountByHash
  , ethGetBlockTransactionCountByNumber
  , ethGetUncleCountByBlockHash
  , ethGetUncleCountByBlockNumber
  , ethGetCode
  , ethSign
  , ethSendTransaction
  ) where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Argonaut.Core (Json, isBoolean, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.ByteString as B
import Data.Either (Either(Right, Left), either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Ethereum.Text (fromHex, toHex)
import Ethereum.Type as E
import Network.Rpc.Json as Rpc

type Decoder r = Json -> Either String r
type DefaultBlock = Either E.BlockNumber E.Tag

data EthF more = Web3ClientVersion (Decoder String) (String -> more)
               | Web3Keccak256 B.ByteString (Decoder E.Keccak256) (E.Keccak256 -> more)
               | NetVersion (Decoder E.Network) (E.Network -> more)
               | NetListening (Decoder Boolean) (Boolean -> more)
               | NetPeerCount (Decoder E.Quantity) (E.Quantity -> more)
               | EthProtocolVersion (Decoder String) (String -> more)
               | EthSyncing (Decoder (Maybe E.SyncStatus)) (Maybe E.SyncStatus -> more)
               | EthCoinbase (Decoder E.Address) (E.Address -> more)
               | EthMining (Decoder Boolean) (Boolean -> more)
               | EthHashrate (Decoder E.Quantity) (E.Quantity -> more)
               | EthGasPrice (Decoder E.Wei) (E.Wei -> more)
               | EthAccounts (Decoder (Array E.Address)) (Array E.Address -> more)
               | EthBlockNumber (Decoder E.BlockNumber) (E.BlockNumber -> more)
               | EthGetBalance E.Address DefaultBlock (Decoder E.Wei) (E.Wei -> more)
               | EthGetStorageAt E.Address Int DefaultBlock (Decoder E.Bytes) (E.Bytes -> more)
               | EthGetTxCount E.Address DefaultBlock (Decoder (Maybe E.Quantity)) ((Maybe E.Quantity) -> more)
               | EthGetBlockTxCountByHash E.BlockHash (Decoder (Maybe E.Quantity)) (Maybe E.Quantity -> more)
               | EthGetBlockTxCountByNumber DefaultBlock (Decoder (Maybe E.Quantity)) (Maybe E.Quantity -> more)
               | EthGetUncleCountByBlockHash E.BlockHash (Decoder (Maybe E.Quantity)) ((Maybe E.Quantity) -> more)
               | EthGetUncleCountByBlockNumber DefaultBlock (Decoder (Maybe E.Quantity)) ((Maybe E.Quantity) -> more)
               | EthGetCode E.Address DefaultBlock (Decoder E.Code) (E.Code -> more)
               | EthSign E.Address B.ByteString (Decoder E.Signature) (E.Signature -> more)
               | EthSendTransaction E.Transaction (Decoder (Maybe E.TxHash)) (Maybe E.TxHash -> more)

type Eth a = Free EthF a

-- | Current client version
web3ClientVersion :: Eth String
web3ClientVersion = liftF $ Web3ClientVersion decodeJson id

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data
keccak256 :: B.ByteString -> Eth E.Keccak256
keccak256 s = liftF $ Web3Keccak256 s decodeJson id

-- | Current network
netVersion :: Eth E.Network
netVersion = liftF $ NetVersion decodeJson id

-- | If client is actively listening for network connections
netListening :: Eth Boolean
netListening = liftF $ NetListening decodeJson id

-- | Number of peers currently connected to the client
netPeerCount :: Eth E.Quantity
netPeerCount = liftF $ NetPeerCount decodeJson id

-- | Current ethereum protocol version
ethProtocolVersion :: Eth String
ethProtocolVersion = liftF $ EthProtocolVersion decodeJson id

-- | Syncrhronization status
ethSyncing :: Eth (Maybe E.SyncStatus)
ethSyncing = liftF $ EthSyncing decoder id
  where decoder = decodeJson >>> map \(SyncStatusResponse r) -> r

-- | Client coinbase address
ethCoinbase :: Eth E.Address
ethCoinbase = liftF $ EthCoinbase decodeJson id

-- | If client is actively mining new blocks
ethMining :: Eth Boolean
ethMining = liftF $ EthMining decodeJson id

-- | Number of hashes per second that the node is mining with
ethHashrate :: Eth E.Quantity
ethHashrate = liftF $ EthHashrate decodeJson id

-- | Current price per gas in WEI
ethGasPrice :: Eth E.Wei
ethGasPrice = liftF $ EthGasPrice decodeJson id

-- | List of addresses owned by client
ethAccounts :: Eth (Array E.Address)
ethAccounts = liftF $ EthAccounts decodeJson id

-- | Number of most recent block
ethBlockNumber :: Eth E.BlockNumber
ethBlockNumber = liftF $ EthBlockNumber decodeJson id

-- | Balance of the account of given address
ethGetBalance :: E.Address -> DefaultBlock -> Eth E.Wei
ethGetBalance address defBlock =
  liftF $ EthGetBalance address defBlock decodeJson id

-- | Value from a storage position at a given address
ethGetStorageAt :: E.Address -> Int -> DefaultBlock -> Eth E.Bytes
ethGetStorageAt address pos defBlock =
  liftF $ EthGetStorageAt address pos defBlock decodeJson id

-- | Number of transactions sent from an address
ethGetTransactionCount :: E.Address -> DefaultBlock -> Eth (Maybe E.Quantity)
ethGetTransactionCount address defBlock =
  liftF $ EthGetTxCount address defBlock decoder id
  where decoder = decodeJson >=> traverse fromHex

-- | Number of transactions in a block from a block matching the given block hash
ethGetBlockTransactionCountByHash :: E.BlockHash -> Eth (Maybe E.Quantity)
ethGetBlockTransactionCountByHash block =
  liftF $ EthGetBlockTxCountByHash block decoder id
  where decoder = decodeJson >=> traverse fromHex

-- | Number of transactions in a block from a block matching the given block number
ethGetBlockTransactionCountByNumber :: DefaultBlock -> Eth (Maybe E.Quantity)
ethGetBlockTransactionCountByNumber block =
  liftF $ EthGetBlockTxCountByNumber block decoder id
  where decoder = decodeJson >=> traverse fromHex

-- | Number of uncles in a block from a block matching the given block hash
ethGetUncleCountByBlockHash :: E.BlockHash -> Eth (Maybe E.Quantity)
ethGetUncleCountByBlockHash blockHash =
  liftF $ EthGetUncleCountByBlockHash blockHash decoder id
  where decoder = decodeJson >=> traverse fromHex

-- | Number of uncles in a block from a block matching the given block number
ethGetUncleCountByBlockNumber :: DefaultBlock -> Eth (Maybe E.Quantity)
ethGetUncleCountByBlockNumber defBlock =
  liftF $ EthGetUncleCountByBlockNumber defBlock decoder id
  where decoder = decodeJson >=> traverse fromHex

-- | Code at a given address
ethGetCode :: E.Address -> DefaultBlock -> Eth E.Code
ethGetCode address defBlock =
  liftF $ EthGetCode address defBlock decodeJson id

{-
     Calculate an Ethereum specific signature with:
     sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message)))
-}
ethSign :: E.Address -> B.ByteString -> Eth E.Signature
ethSign address message = liftF $ EthSign address message decodeJson id

-- | Creates new message call transaction or a contract creation, if the data field contains code
ethSendTransaction :: E.Transaction -> Eth (Maybe E.TxHash)
ethSendTransaction tx =
  liftF $ EthSendTransaction tx decoder id
  where decoder json = do
          s <- decodeJson json
          if (s == "0x0")
            then pure Nothing
            else Just <$> fromHex s


-- | Runs Eth monad returning Aff
run :: ∀ c e a. Rpc.Transport c e => c -> Eth a -> Aff e a
run = foldFree <<< nt
  where
    nt :: Rpc.Transport c e => c -> EthF ~> Aff e
    nt cfg (Web3ClientVersion d f) =
      call0 cfg "web3_clientVersion" >>= handle d f
    nt cfg (Web3Keccak256 s d f) =
      let request = Rpc.Request { id: 1, method: "web3_sha3", params: [toHex s] }
      in Rpc.call cfg request >>= handle d f
    nt cfg (NetVersion d f) =
      call0 cfg "net_version" >>= handle d f
    nt cfg (NetListening d f) =
      call0 cfg "net_listening" >>= handle d f
    nt cfg (NetPeerCount d f) =
      call0 cfg "net_peerCount" >>= handle d f
    nt cfg (EthProtocolVersion d f) =
      call0 cfg "eth_protocolVersion" >>= handle d f
    nt cfg (EthSyncing d f) =
      call0 cfg "eth_syncing" >>= handle d f
    nt cfg (EthCoinbase d f) =
      call0 cfg "eth_coinbase" >>= handle d f
    nt cfg (EthMining d f) =
      call0 cfg "eth_mining" >>= handle d f
    nt cfg (EthHashrate d f) =
      call0 cfg "eth_hashrate" >>= handle d f
    nt cfg (EthGasPrice d f) =
      call0 cfg "eth_gasPrice" >>= handle d f
    nt cfg (EthAccounts d f) =
      call0 cfg "eth_accounts" >>= handle d f
    nt cfg (EthBlockNumber d f) =
      call0 cfg "eth_blockNumber" >>= handle d f
    nt cfg (EthGetBalance address defaultBlock d f) =
      let param1 = toHex address
          param2 = packDefaultBlock defaultBlock
          request = Rpc.Request { id: 1
                                , method: "eth_getBalance"
                                , params: [param1, param2]
                                }
      in Rpc.call cfg request >>= handle d f
    nt cfg (EthGetStorageAt address pos defaultBlock d f) =
      let param1 = toHex address
          param2 = toHex pos
          param3 = packDefaultBlock defaultBlock
          request = Rpc.Request { id: 1
                                , method: "eth_getStorageAt"
                                , params: [param1, param2, param3]
                                }
      in Rpc.call cfg request >>= handle d f
    nt cfg (EthGetTxCount address defaultBlock d f) =
      let param1 = toHex address
          param2 = packDefaultBlock defaultBlock
          request = Rpc.Request { id: 1
                                , method: "eth_getTransactionCount"
                                , params: [param1, param2]
                                }
      in Rpc.call cfg request >>= handle d f
    nt cfg (EthGetBlockTxCountByHash blockHash d f) =
      let request = Rpc.Request { id: 1
                                , method: "eth_getBlockTransactionCountByHash"
                                , params: [toHex blockHash]
                                }
      in Rpc.call cfg request >>= handle d f
    nt cfg (EthGetBlockTxCountByNumber defaultBlock d f) =
      let request = Rpc.Request { id: 1
                                , method: "eth_getBlockTransactionCountByNumber"
                                , params: [packDefaultBlock defaultBlock]
                                }
      in Rpc.call cfg request >>= handle d f
    nt cfg (EthGetUncleCountByBlockHash blockHash d f) =
      let request = Rpc.Request { id: 1
                                , method: "eth_getUncleCountByBlockHash"
                                , params: [toHex blockHash]
                                }
      in Rpc.call cfg request >>= handle d f
    nt cfg (EthGetUncleCountByBlockNumber defaultBlock d f) =
      let request = Rpc.Request { id: 1
                                , method: "eth_getUncleCountByBlockNumber"
                                , params: [packDefaultBlock defaultBlock]
                                }
      in Rpc.call cfg request >>= handle d f
    nt cfg (EthGetCode address defaultBlock d f) =
      let request = Rpc.Request { id: 1
                                , method: "eth_getCode"
                                , params: [packDefaultBlock defaultBlock]
                                }
      in Rpc.call cfg request >>= handle d f
    nt cfg (EthSign address message d f) =
      let request = Rpc.Request { id: 1
                                , method: "eth_sign"
                                , params: [toHex address, toHex message]
                                }
      in Rpc.call cfg request >>= handle d f
    nt cfg (EthSendTransaction tx d f) =
      let request = Rpc.Request { id: 1
                                , method: "eth_sendTransaction"
                                , params: [stringify $ encodeJson tx]
                                }
      in Rpc.call cfg request >>= handle d f


    call0 :: ∀ r. Rpc.Transport r e => r -> Rpc.Method -> Aff e (Rpc.Response Json)
    call0 c m = Rpc.call c $ Rpc.Request { id: 1, method: m, params: [] }

    unpackRpcResponse :: ∀ fx. Rpc.Response Json -> Aff fx Json
    unpackRpcResponse (Rpc.Response (Right json)) = pure json
    unpackRpcResponse (Rpc.Response (Left (Rpc.Error e))) =
      err $ "JSON RPC error (" <> (show e.code) <> "): " <> e.message

    packDefaultBlock :: DefaultBlock -> String
    packDefaultBlock (Left block) = toHex block
    packDefaultBlock (Right tag) = show tag

    decode :: ∀ fx r. Decoder r -> Json -> Aff fx r
    decode decoder json = either err pure $ decoder json

    handle :: ∀ r fx b. Decoder r -> (r -> b) -> Rpc.Response Json -> Aff fx b
    handle decoder continue response = unpackRpcResponse response >>= decode decoder <#> continue

    err :: ∀ fx b. String -> Aff fx b
    err = throwError <<< error

newtype SyncStatusResponse = SyncStatusResponse (Maybe E.SyncStatus)

instance decodeSyncStatusResponse :: DecodeJson SyncStatusResponse
  where decodeJson json
          | isBoolean json = pure $ SyncStatusResponse Nothing
          | otherwise = SyncStatusResponse <<< Just <$> decodeJson json
