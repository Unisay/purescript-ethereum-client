module Ethereum.Api (
    EthF
  , Eth
  , Network
  , runEth
  , netVersion
  , netListening
  , netPeerCount
  , clientVersion
  , keccak256
  ) where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.ByteString (ByteString)
import Data.Maybe (maybe)
import Ethereum.Rpc as Rpc
import Ethereum.Text (fromHex, fromHexQuantity, toHex)
import Network.HTTP.Affjax (AJAX, URL)

data EthF more = Web3ClientVersion (String -> more)
               | Keccak256 ByteString (ByteString -> more)
               | NetVersion (Network -> more)
               | NetListening (Boolean -> more)
               | NetPeerCount (Int -> more)

type Eth a = Free EthF a

-- | Current client version
clientVersion :: Eth String
clientVersion = liftF $ Web3ClientVersion id

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data
keccak256 :: ByteString -> Eth ByteString
keccak256 s = liftF $ Keccak256 s id

data Network = Mainnet
             | Morden
             | Ropsten
             | Rinkeby
             | Kovan
             | UnknownNet String

instance showNetwork :: Show Network where
  show Mainnet = "Ethereum Mainnet"
  show Morden = "Morden Testnet"
  show Ropsten = "Ropsten Testnet"
  show Rinkeby = "Rinkeby Testnet"
  show Kovan = "Kovan Testnet"
  show (UnknownNet s) = "Unknown network: " <> s

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

-- | Runs Eth monad returning Aff
runEth :: ∀ e a. URL -> Eth a -> Aff (ajax :: AJAX | e) a
runEth url = foldFree (fromEthF url)

fromResp :: ∀ r e a. (r -> Aff e a) -> Rpc.Response r -> Aff e a
fromResp f (Rpc.Result r) = f r
fromResp f (Rpc.Error code message) = throwError $
  error $ "JSON RPC error (" <> (show code) <> "): " <> message

parseQuantity :: ∀ e. String -> Aff e Int
parseQuantity = maybe (throwError $ error "Failed to parse HEX QUANTITY") pure <<< fromHexQuantity

fromEthF :: ∀ e. URL -> EthF ~> Aff (ajax :: AJAX | e)
fromEthF url (Web3ClientVersion f) = do
  r <- Rpc.call url (Rpc.method "web3_clientVersion")
  fromResp (pure <<< f) r
fromEthF url (Keccak256 s f) = do
  r <- Rpc.call url (Rpc.Request { method: "web3_sha3", params: [toHex s] })
  fromResp (pure <<< f <<< fromHex) r
fromEthF url (NetVersion f) = do
  r <- Rpc.call url (Rpc.method "net_version")
  fromResp (pure <<< f <<< parseNetwork) r
fromEthF url (NetListening f) = do
  r <- Rpc.call url (Rpc.method "net_listening")
  fromResp (pure <<< f) r
fromEthF url (NetPeerCount f) = do
  r <- Rpc.call url (Rpc.method "net_peerCount")
  fromResp (pure <<< f <=< parseQuantity) r
