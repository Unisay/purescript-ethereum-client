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
  ) where

import Prelude
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Argonaut.Core (Json, isBoolean)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.ByteString (ByteString)
import Data.Either (Either, either, note)
import Data.Maybe (Maybe(Just, Nothing))
import Ethereum.Rpc as Rpc
import Ethereum.Text (fromHex, fromHexQuantity', toHex)
import Ethereum.Type (Address, Network(..), SyncStatus, Wei(Wei))

type Decoder r = Json -> Either String r

data EthF more = Web3ClientVersion (Decoder String) (String -> more)
               | Keccak256 ByteString (Decoder ByteString) (ByteString -> more)
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

type Eth a = Free EthF a

-- | Current client version
web3ClientVersion :: Eth String
web3ClientVersion = liftF $ Web3ClientVersion decodeJson id

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data
keccak256 :: ByteString -> Eth ByteString
keccak256 s = liftF $ Keccak256 s (decodeJson >>> map fromHex) id

-- | Current network
netVersion :: Eth Network
netVersion = liftF $ NetVersion (decodeJson >>> map parseNetwork) id

-- | If client is actively listening for network connections
netListening :: Eth Boolean
netListening = liftF $ NetListening decodeJson id

-- | Number of peers currently connected to the client
netPeerCount :: Eth Int
netPeerCount = liftF $ NetPeerCount (decodeJson >=> parseSmallInt) id

-- | Current ethereum protocol version
ethProtocolVersion :: Eth String
ethProtocolVersion = liftF $ EthProtocolVersion decodeJson id

-- | Syncrhronization status
ethSyncing :: Eth (Maybe SyncStatus)
ethSyncing = liftF $ EthSyncing (decodeJson >>> map \(SyncStatusResponse r) -> r) id

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

-- | Runs Eth monad returning Aff
run :: ∀ c e a. Rpc.Transport c e => c -> Eth a -> Aff e a
run = foldFree <<< nt
  where
    nt :: Rpc.Transport c e => c -> EthF ~> Aff e
    nt cfg (Web3ClientVersion d f) = Rpc.call0 cfg "web3_clientVersion" >>= handle d f
    nt cfg (Keccak256 s d f) = Rpc.callParams cfg "web3_sha3" [toHex s] >>= handle d f
    nt cfg (NetVersion d f) = Rpc.call0 cfg "net_version" >>= handle d f
    nt cfg (NetListening d f) = Rpc.call0 cfg "net_listening" >>= handle d f
    nt cfg (NetPeerCount d f) = Rpc.call0 cfg "net_peerCount" >>= handle d f
    nt cfg (EthProtocolVersion d f) = Rpc.call0 cfg "eth_protocolVersion" >>= handle d f
    nt cfg (EthSyncing d f) = Rpc.call0 cfg "eth_syncing" >>= handle d f
    nt cfg (EthCoinbase d f) = Rpc.call0 cfg "eth_coinbase" >>= handle d f
    nt cfg (EthMining d f) = Rpc.call0 cfg "eth_mining" >>= handle d f
    nt cfg (EthHashrate d f) = Rpc.call0 cfg "eth_hashrate" >>= handle d f
    nt cfg (EthGasPrice d f) = Rpc.call0 cfg "eth_gasPrice" >>= handle d f
    nt cfg (EthAccounts d f) = Rpc.call0 cfg "eth_accounts" >>= handle d f

    unpackRpcResponse :: ∀ fx. Rpc.Response Json -> Aff fx Json
    unpackRpcResponse (Rpc.Result json) = pure json
    unpackRpcResponse (Rpc.Error code message) = err $ "JSON RPC error (" <> (show code) <> "): " <> message

    decode :: ∀ fx r. Decoder r -> Json -> Aff fx r
    decode decoder json = either err pure $ decoder json

    handle :: ∀ r fx b. Decoder r -> (r -> b) -> Rpc.Response Json -> Aff fx b
    handle decoder continue response = unpackRpcResponse response >>= decode decoder <#> continue

    err :: ∀ fx b. String -> Aff fx b
    err = throwError <<< error

parseSmallInt :: String -> Either String Int
parseSmallInt = note "Failed to parse hex string as int" <<< fromHexQuantity'

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
