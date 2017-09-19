module Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Ethereum.Api.Web3 (clientVersion, runEth)
import Network.HTTP.Affjax (AJAX)

main :: ∀ e. Eff ( ajax :: AJAX, console :: CONSOLE | e) Unit
main = launchAff_ $ do
  version <- runEth "http://127.0.0.1:8545" clientVersion
  liftEff $ log $ version
