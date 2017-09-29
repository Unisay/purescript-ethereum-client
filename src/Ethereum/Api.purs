module Ethereum.Api
  ( EthF
  , Eth
  , module Ethereum.Type
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
  ) where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Argonaut.Core (Json, isBoolean)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.ByteString as BS
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (traverse)
import Ethereum.Text (fromHex, toHex)
import Ethereum.Type (Address(..), BlockHash(..), BlockNumber(..), Network(..), Quantity(..), SyncStatus, Tag(..), Wei(Wei))
import Network.Rpc.Json as Rpc

type Decoder r = Json -> Either String r
type DefaultBlock = Either BlockNumber Tag

data EthF more = Web3ClientVersion (Decoder String) (String -> more)
               | Keccak256 BS.ByteString (Decoder BS.ByteString) (BS.ByteString -> more)
               | NetVersion (Decoder Network) (Network -> more)
               | NetListening (Decoder Boolean) (Boolean -> more)
               | NetPeerCount (Decoder Int) (Int -> more)
               | EthProtocolVersion (Decoder String) (String -> more)
               | EthSyncing (Decoder (Maybe SyncStatus)) (Maybe SyncStatus -> more)
               | EthCoinbase (Decoder Address) (Address -> more)
               | EthMining (Decoder Boolean) (Boolean -> more)
               | EthHashrate (Decoder Int) (Int -> more)
               | EthGasPrice (Decoder Wei) (Wei -> more)
               | EthAccounts (Decoder (Array Address)) (Array Address -> more)
               | EthBlockNumber (Decoder BlockNumber) (BlockNumber -> more)
               | EthGetBalance Address DefaultBlock (Decoder (Maybe Wei)) ((Maybe Wei) -> more)
               | EthGetStorageAt Address Int DefaultBlock (Decoder BS.ByteString) (BS.ByteString -> more)
               | EthGetTxCount Address DefaultBlock (Decoder (Maybe Quantity)) ((Maybe Quantity) -> more)
               | EthGetBlockTxCountByHash BlockHash (Decoder (Maybe Quantity)) (Maybe Quantity -> more)
               | EthGetBlockTxCountByNumber DefaultBlock (Decoder (Maybe Quantity)) (Maybe Quantity -> more)
               | EthGetUncleCountByBlockHash BlockHash (Decoder (Maybe Quantity)) ((Maybe Quantity) -> more)
               | EthGetUncleCountByBlockNumber DefaultBlock (Decoder (Maybe Quantity)) ((Maybe Quantity) -> more)
               | EthGetCode Address DefaultBlock (Decoder (Maybe BS.ByteString)) ((Maybe BS.ByteString) -> more)

type Eth a = Free EthF a

-- | Current client version
web3ClientVersion :: Eth String
web3ClientVersion = liftF $ Web3ClientVersion decodeJson id

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data
keccak256 :: BS.ByteString -> Eth BS.ByteString
keccak256 s = liftF $ Keccak256 s decoder id
  where decoder = decodeJson >=> note "Invalid HEX string" <<< fromHex

-- | Current network
netVersion :: Eth Network
netVersion = liftF $ NetVersion decoder id
  where decoder = decodeJson >>> map parseNetwork

-- | If client is actively listening for network connections
netListening :: Eth Boolean
netListening = liftF $ NetListening decodeJson id

-- | Number of peers currently connected to the client
netPeerCount :: Eth Int
netPeerCount = liftF $ NetPeerCount decoder id
  where decoder = decodeJson >=> parseSmallInt

-- | Current ethereum protocol version
ethProtocolVersion :: Eth String
ethProtocolVersion = liftF $ EthProtocolVersion decodeJson id

-- | Syncrhronization status
ethSyncing :: Eth (Maybe SyncStatus)
ethSyncing = liftF $ EthSyncing decoder id
  where decoder = decodeJson >>> map \(SyncStatusResponse r) -> r

-- | Client coinbase address
ethCoinbase :: Eth Address
ethCoinbase = liftF $ EthCoinbase decodeJson id

-- | If client is actively mining new blocks
ethMining :: Eth Boolean
ethMining = liftF $ EthMining decodeJson id

-- | Number of hashes per second that the node is mining with
ethHashrate :: Eth Int
ethHashrate = liftF $ EthHashrate (decodeJson >=> parseSmallInt) id

-- | Current price per gas in WEI
ethGasPrice :: Eth Wei
ethGasPrice = liftF $ EthGasPrice decodeJson id

-- | List of addresses owned by client
ethAccounts :: Eth (Array Address)
ethAccounts = liftF $ EthAccounts decodeJson id

-- | Number of most recent block
ethBlockNumber :: Eth BlockNumber
ethBlockNumber = liftF $ EthBlockNumber decoder id
  where decoder = decodeJson >=> note "Invalid HEX number" <<< fromHex

-- | Balance of the account of given address
ethGetBalance :: Address -> DefaultBlock -> Eth (Maybe Wei)
ethGetBalance address defBlock = liftF $ EthGetBalance address defBlock decodeJson id

-- | Value from a storage position at a given address
ethGetStorageAt :: Address -> Int -> DefaultBlock -> Eth BS.ByteString
ethGetStorageAt address pos defBlock =
  liftF $ EthGetStorageAt address pos defBlock decoder id
  where decoder = decodeJson >=> parseBytes

-- | Number of transactions sent from an address
ethGetTransactionCount :: Address -> DefaultBlock -> Eth (Maybe Quantity)
ethGetTransactionCount address defBlock =
  liftF $ EthGetTxCount address defBlock decoder id
  where decoder = decodeJson >=> traverse parseQuantity

-- | Number of transactions in a block from a block matching the given block hash
ethGetBlockTransactionCountByHash :: BlockHash -> Eth (Maybe Quantity)
ethGetBlockTransactionCountByHash block =
  liftF $ EthGetBlockTxCountByHash block decoder id
  where decoder = decodeJson >=> traverse parseQuantity

-- | Number of transactions in a block from a block matching the given block number
ethGetBlockTransactionCountByNumber :: DefaultBlock -> Eth (Maybe Quantity)
ethGetBlockTransactionCountByNumber block =
  liftF $ EthGetBlockTxCountByNumber block decoder id
  where decoder = decodeJson >=> traverse parseQuantity

-- | Number of uncles in a block from a block matching the given block hash
ethGetUncleCountByBlockHash :: BlockHash -> Eth (Maybe Quantity)
ethGetUncleCountByBlockHash blockHash =
  liftF $ EthGetUncleCountByBlockHash blockHash decoder id
  where decoder = decodeJson >=> traverse parseQuantity

-- | Number of uncles in a block from a block matching the given block number
ethGetUncleCountByBlockNumber :: DefaultBlock -> Eth (Maybe Quantity)
ethGetUncleCountByBlockNumber defBlock =
  liftF $ EthGetUncleCountByBlockNumber defBlock decoder id
  where decoder = decodeJson >=> traverse parseQuantity

-- | Code at a given address
ethGetCode :: Address -> DefaultBlock -> Eth (Maybe BS.ByteString)
ethGetCode address defBlock = liftF $ EthGetCode address defBlock decoder id
  where decoder = decodeJson >=> traverse parseBytes



-- | Runs Eth monad returning Aff
run :: ∀ c e a. Rpc.Transport c e => c -> Eth a -> Aff e a
run = foldFree <<< nt
  where
    nt :: Rpc.Transport c e => c -> EthF ~> Aff e
    nt cfg (Web3ClientVersion d f) =
      call0 cfg "web3_clientVersion" >>= handle d f
    nt cfg (Keccak256 s d f) =
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



    call0 :: ∀ r. Rpc.Transport r e => r -> Rpc.Method -> Aff e (Rpc.Response Json)
    call0 c m = Rpc.call c $ Rpc.Request { id: 1, method: m, params: [] }

    unpackRpcResponse :: ∀ fx. Rpc.Response Json -> Aff fx Json
    unpackRpcResponse (Rpc.Response (Right json)) = pure json
    unpackRpcResponse (Rpc.Response (Left (Rpc.Error e))) = err $ "JSON RPC error (" <> (show e.code) <> "): " <> e.message

    packDefaultBlock :: DefaultBlock -> String
    packDefaultBlock (Left block) = toHex block
    packDefaultBlock (Right tag) = show tag

    decode :: ∀ fx r. Decoder r -> Json -> Aff fx r
    decode decoder json = either err pure $ decoder json

    handle :: ∀ r fx b. Decoder r -> (r -> b) -> Rpc.Response Json -> Aff fx b
    handle decoder continue response = unpackRpcResponse response >>= decode decoder <#> continue

    err :: ∀ fx b. String -> Aff fx b
    err = throwError <<< error

parseSmallInt :: String -> Either String Int
parseSmallInt = note "Failed to parse hex string as int" <<< fromHex

parseQuantity :: String -> Either String Quantity
parseQuantity = parseSmallInt >>> map Quantity

parseBytes :: String -> Either String BS.ByteString
parseBytes = note "Failed to parse hex string as bytes" <<< fromHex

parseNetwork :: String -> Network
parseNetwork "1" = Mainnet
parseNetwork "2" = Morden
parseNetwork "3" = Ropsten
parseNetwork "4" = Rinkeby
parseNetwork "42" = Kovan
parseNetwork s = UnknownNet s

newtype SyncStatusResponse = SyncStatusResponse (Maybe SyncStatus)

instance decodeSyncStatusResponse :: DecodeJson SyncStatusResponse
  where decodeJson json
          | isBoolean json = pure $ SyncStatusResponse Nothing
          | otherwise = SyncStatusResponse <<< Just <$> decodeJson json
