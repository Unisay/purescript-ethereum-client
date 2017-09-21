module Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.ByteString (toUTF8)
import Data.Maybe (maybe)
import Data.String (joinWith)
import Ethereum.Api as E
import Ethereum.Text (toHex)
import Network.HTTP.Affjax (AJAX)

info :: E.Eth String
info = do
  network <- E.netVersion
  listening <- E.netListening
  peers <- E.netPeerCount
  clientVersion <- E.web3ClientVersion
  keccak <- E.keccak256 $ toUTF8 "hello"
  protocolVersion <- E.ethProtocolVersion
  syncStatus <- E.ethSyncing
  coinbase <- E.ethCoinbase
  mining <- E.ethMining
  hashrate <- E.ethHashrate
  gasPrice <- E.ethGasPrice
  accounts <- E.ethAccounts
  pure $ """
  Network:                    """ <> (show network) <> """
  Is listening:               """ <> (show listening) <> """
  Number of Peers:            """ <> (show peers) <> """
  Client version:             """ <> clientVersion <> """
  Keccak 256 (hello):         """ <> (toHex keccak) <> """
  Ethereum protocol version:  """ <> protocolVersion <> """
  Sync status:                """ <> (maybe "Not syncing" show syncStatus) <> """
  Coinbase:                   """ <> (show coinbase) <> """
  Is mining:                  """ <> (show mining) <> """
  Hashes per second:          """ <> (show hashrate) <> """
  Gas price:                  """ <> (show gasPrice) <> """ WEI
  Accounts:                   """ <> (joinWith ", " $ show <$> accounts) <> """
  """

main :: ∀ e. Eff (ajax :: AJAX, console :: CONSOLE | e) Unit
main = launchAff_ $ E.runHttp "http://127.0.0.1:8545" info >>= log
