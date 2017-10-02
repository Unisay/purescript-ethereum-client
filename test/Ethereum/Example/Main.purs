module Ethereum.Example.Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.ByteString (toUTF8)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Ethereum.Api as E
import Ethereum.Text (toHex)
import Network.HTTP.Affjax (AJAX)
import Network.Rpc.Json (AffjaxLoggingTransport(..))
import Test.Unsafe (mkUnsafe)

main :: ∀ e. Eff (console :: CONSOLE, ajax :: AJAX | e) Unit
main = launchAff_ $ E.run transport info >>= log
  where transport = AffjaxLoggingTransport "http://127.0.0.1:8545"

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
  recentBlockNumber <- E.ethBlockNumber
  let defaultBlock = Left recentBlockNumber
  balance <- E.ethGetBalance coinbase defaultBlock
  accountTxCount <- E.ethGetTransactionCount coinbase defaultBlock
  let blockHash = mkUnsafe "b903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238"
  blockTxCountByHash <- E.ethGetBlockTransactionCountByHash blockHash
  blockTxCountByBlock <- E.ethGetBlockTransactionCountByNumber defaultBlock
  uncleCountByHash <- E.ethGetUncleCountByBlockHash blockHash
  uncleCountByBlock <- E.ethGetUncleCountByBlockNumber defaultBlock
  let address = mkUnsafe "5481c0fe170641bd2e0ff7f04161871829c1902d"
  code <- E.ethGetCode address defaultBlock
  pure $ """
Network:                           """ <> show network <> """
Is listening:                      """ <> show listening <> """
Number of Peers:                   """ <> show peers <> """
Client version:                    """ <> clientVersion <> """
Keccak 256 (hello):                """ <> toHex keccak <> """
Ethereum protocol version:         """ <> protocolVersion <> """
Sync status:                       """ <> maybe "Not syncing" show syncStatus <> """
Coinbase:                          """ <> show coinbase <> """
Is mining:                         """ <> show mining <> """
Hashes per second:                 """ <> show hashrate <> """
Gas price:                         """ <> show gasPrice <> """
Accounts:                          """ <> joinWith ", " (show <$> accounts) <> """
Most recent block:                 """ <> show recentBlockNumber <> """
Account balance:                   """ <> show balance <> """
# of coinbase transactions:        """ <> show (maybe 0 unwrap accountTxCount) <> """
# of transactions by hash:         """ <> show (maybe 0 unwrap blockTxCountByHash) <> """
# of transactions by block:        """ <> show (maybe 0 unwrap blockTxCountByBlock) <> """
# of uncles in a block by hash:    """ <> show (maybe 0 unwrap uncleCountByHash) <> """
# of uncles in a block by block:   """ <> show (maybe 0 unwrap uncleCountByBlock) <> """
Contract code:                     """ <> show code <> """
"""
