module Ethereum.Api.Web3 (
    EthF
  , Eth
  , runEth
  , clientVersion
  , keccak256
  ) where

import Prelude
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.ByteString (ByteString)
import Ethereum.Api.Text (fromHex, toHex)
import Ethereum.Rpc as Rpc
import Network.HTTP.Affjax (AJAX, URL)

data EthF more = Web3ClientVersion (String -> more)
               | Keccak256 ByteString (ByteString -> more)

type Eth a = Free EthF a

-- | Returns the current client version
clientVersion :: Eth String
clientVersion = liftF $ Web3ClientVersion id

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data
keccak256 :: ByteString -> Eth ByteString
keccak256 s = liftF $ Keccak256 s id

-- | Runs Eth monad returning Aff
runEth :: ∀ e a. URL -> Eth a -> Aff (ajax :: AJAX | e) a
runEth url = foldFree (fromEthF url)

fromResp :: ∀ e a. (String -> a) -> Rpc.Response -> Aff (ajax :: AJAX | e) a
fromResp f (Rpc.Result result) = pure $ f result
fromResp f (Rpc.Error code message) = throwError $
  error $ "JSON RPC error (" <> (show code) <> "): " <> message

fromEthF :: ∀ e. URL -> EthF ~> Aff (ajax :: AJAX | e)
fromEthF url (Web3ClientVersion f) = do
  r <- Rpc.call url (Rpc.method "web3_clientVersion")
  fromResp f r
fromEthF url (Keccak256 s f) = do
  r <- Rpc.call url (Rpc.Request { method: "web3_sha3", params: [toHex s] })
  fromResp (f <<< fromHex) r
