module Ethereum.Api.Web3 (
    EthF
  , runEth
  , clientVersion
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Free (Free, foldFree, liftF)
import Network.HTTP.Affjax (AJAX, URL)
import Ethereum.Rpc as Rpc

type ClientVersion = String

data EthF more =
  Web3ClientVersion (ClientVersion -> more)

type Eth a = Free EthF a

clientVersion :: Eth String
clientVersion = liftF $ Web3ClientVersion id

fromEthF :: ∀ e. URL -> EthF ~> Aff (ajax :: AJAX | e)
fromEthF url (Web3ClientVersion f) = Rpc.call url (Rpc.method "web3_clientVersion")
                                   <#> (\(Rpc.Response r) -> f r.result)

runEth :: ∀ e a. URL -> Eth a -> Aff (ajax :: AJAX | e) a
runEth url = foldFree (fromEthF url)
