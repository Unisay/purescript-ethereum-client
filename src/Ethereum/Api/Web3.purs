module Ethereum.Api.Web3 (
    EthF
  , Eth
  , runEth
  , clientVersion
  , keccak256
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Free (Free, foldFree, liftF)
import Ethereum.Rpc as Rpc
import Network.HTTP.Affjax (AJAX, URL)

data EthF more = Web3ClientVersion (String -> more)
               | Keccak256 String (String -> more)

type Eth a = Free EthF a

-- | Returns the current client version
clientVersion :: Eth String
clientVersion = liftF $ Web3ClientVersion id

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data
keccak256 :: String -> Eth String
keccak256 s = liftF $ Keccak256 s id

-- | Runs Eth monad returning Aff
runEth :: ∀ e a. URL -> Eth a -> Aff (ajax :: AJAX | e) a
runEth url = foldFree (fromEthF url)

result :: ∀ a. (String -> a) -> Rpc.Response -> a
result f (Rpc.Response r) = f r.result

fromEthF :: ∀ e. URL -> EthF ~> Aff (ajax :: AJAX | e)
fromEthF url (Web3ClientVersion f) = Rpc.call url (Rpc.method "web3_clientVersion") <#> result f
fromEthF url (Keccak256 s f) = Rpc.call url (Rpc.Request { method: "web3_sha3", params: [s] }) <#> result f
