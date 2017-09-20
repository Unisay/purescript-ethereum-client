module Ethereum.Api (
    EthF
  , Eth
  , module Ethereum.Type
  , runEth
  , netVersion
  , netListening
  , netPeerCount
  , clientVersion
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
import Data.Argonaut.Core (isBoolean)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.BigInt (BigInt)
import Data.ByteString (ByteString)
import Data.Maybe (Maybe(..), maybe)
import Ethereum.Rpc as Rpc
import Ethereum.Text (fromHex, fromHexQuantity, fromHexQuantity', toHex)
import Ethereum.Type (Address, Network(..), SyncStatus, Wei(Wei))
import Network.HTTP.Affjax (AJAX, URL)

data EthF more = Web3ClientVersion (String -> more)
               | Keccak256 ByteString (ByteString -> more)
               | NetVersion (Network -> more)
               | NetListening (Boolean -> more)
               | NetPeerCount (Int -> more)
               | EthProtocolVersion (String -> more)
               | EthSyncing (Maybe SyncStatus -> more)
               | EthCoinbase (Address -> more)
               | EthMining (Boolean -> more)
               | EthHashrate (Int -> more)
               | EthGasPrice (Wei -> more)
               | EthAccounts (Array Address -> more)

type Eth a = Free EthF a

-- | Current client version
clientVersion :: Eth String
clientVersion = liftF $ Web3ClientVersion id

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data
keccak256 :: ByteString -> Eth ByteString
keccak256 s = liftF $ Keccak256 s id

parseNetwork :: String -> Network
parseNetwork "1" = Mainnet
parseNetwork "2" = Morden
parseNetwork "3" = Ropsten
parseNetwork "4" = Rinkeby
parseNetwork "42" = Kovan
parseNetwork s = UnknownNet s

-- | Current network
netVersion :: Eth Network
netVersion = liftF $ NetVersion id

-- | If client is actively listening for network connections
netListening :: Eth Boolean
netListening = liftF $ NetListening id

-- | Number of peers currently connected to the client
netPeerCount :: Eth Int
netPeerCount = liftF $ NetPeerCount id

-- | Current ethereum protocol version
ethProtocolVersion :: Eth String
ethProtocolVersion = liftF $ EthProtocolVersion id

-- | Syncrhronization status
ethSyncing :: Eth (Maybe SyncStatus)
ethSyncing = liftF $ EthSyncing id

-- | Client coinbase address
ethCoinbase :: Eth Address
ethCoinbase = liftF $ EthCoinbase id

-- | If client is actively mining new blocks
ethMining :: Eth Boolean
ethMining = liftF $ EthMining id

-- | Number of hashes per second that the node is mining with
ethHashrate :: Eth Int
ethHashrate = liftF $ EthHashrate id

-- | Current price per gas in WEI
ethGasPrice :: Eth Wei
ethGasPrice = liftF $ EthGasPrice id

-- | List of addresses owned by client
ethAccounts :: Eth (Array Address)
ethAccounts = liftF $ EthAccounts id

-- | Runs Eth monad returning Aff
runEth :: ∀ e a. URL -> Eth a -> Aff (ajax :: AJAX | e) a
runEth url = foldFree (fromEthF url)

fromEthF :: ∀ e. URL -> EthF ~> Aff (ajax :: AJAX | e)
fromEthF url (Web3ClientVersion f) =
  Rpc.call url (Rpc.method "web3_clientVersion") >>= fromResp (pure <<< f)
fromEthF url (Keccak256 s f) =
  Rpc.call url (Rpc.Request { method: "web3_sha3", params: [toHex s] }) >>= fromResp (pure <<< f <<< fromHex)
fromEthF url (NetVersion f) =
  Rpc.call url (Rpc.method "net_version") >>= fromResp (pure <<< f <<< parseNetwork)
fromEthF url (NetListening f) =
  Rpc.call url (Rpc.method "net_listening") >>= fromResp (pure <<< f)
fromEthF url (NetPeerCount f) = do
  Rpc.call url (Rpc.method "net_peerCount") >>= fromResp (pure <<< f <=< parseSmallInt)
fromEthF url (EthProtocolVersion f) = do
  Rpc.call url (Rpc.method "eth_protocolVersion") >>= fromResp (pure <<< f)
fromEthF url (EthSyncing f) =
  Rpc.call url (Rpc.method "eth_syncing") >>= fromResp (pure <<< f <<< \(SyncStatusResponse r) -> r)
fromEthF url (EthCoinbase f) =
  Rpc.call url (Rpc.method "eth_coinbase") >>= fromResp (pure <<< f)
fromEthF url (EthMining f) =
  Rpc.call url (Rpc.method "eth_mining") >>= fromResp (pure <<< f)
fromEthF url (EthHashrate f) =
  Rpc.call url (Rpc.method "eth_hashrate") >>= fromResp (pure <<< f <=< parseSmallInt)
fromEthF url (EthGasPrice f) =
  Rpc.call url (Rpc.method "eth_gasPrice") >>= fromResp (pure <<< f)
fromEthF url (EthAccounts f) =
  Rpc.call url (Rpc.method "eth_accounts") >>= fromResp (pure <<< f)



err :: ∀ e a. String -> Aff e a
err = throwError <<< error

fromResp :: ∀ r e a. (r -> Aff e a) -> Rpc.Response r -> Aff e a
fromResp f (Rpc.Result r) = f r
fromResp f (Rpc.Error code message) = err $ "JSON RPC error (" <> (show code) <> "): " <> message

parseBigInt :: ∀ e. String -> Aff e BigInt
parseBigInt = maybe (err "Failed to parse hex string as big int") pure <<< fromHexQuantity

parseSmallInt :: ∀ e. String -> Aff e Int
parseSmallInt = maybe (err "Failed to parse hex string as int") pure <<< fromHexQuantity'

newtype SyncStatusResponse = SyncStatusResponse (Maybe SyncStatus)

instance decodeSyncStatusResponse :: DecodeJson SyncStatusResponse
  where decodeJson json
          | isBoolean json = pure $ SyncStatusResponse Nothing
          | otherwise = SyncStatusResponse <<< Just <$> decodeJson json
