module Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Ethereum.Api.Web3 (Eth, clientVersion, keccak256, runEth)
import Network.HTTP.Affjax (AJAX)

info :: Eth String
info = do
  version <- clientVersion
  keccak <- keccak256 "hello"
  pure $ """
  Client version:      $version
  Keccak 256 (hello):  $keccak
  """

main :: ∀ e. Eff ( ajax :: AJAX, console :: CONSOLE | e) Unit
main = launchAff_ $ runEth "http://127.0.0.1:8545" info >>= log
