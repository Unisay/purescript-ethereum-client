module Ethereum.Example.Spec where

import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.ByteString (toUTF8)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.String (joinWith)
import Ethereum.Api as E
import Ethereum.Text (toHex)
import Network.HTTP.Affjax (AJAX)
import Network.Rpc.Json (AffjaxLoggingTransport(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unsafe (mkUnsafe)

main :: ∀ e. Eff ( console    :: CONSOLE
                 , testOutput :: TESTOUTPUT
                 , avar       :: AVAR
                 , random     :: RANDOM
                 , ajax       :: AJAX
                 | e
                 ) Unit
main = runTest spec

spec :: ∀ e. TestSuite ( ajax :: AJAX, console :: CONSOLE | e)
spec = do
  suite "Example" $
    test "against local node" do
      let transport = AffjaxLoggingTransport "http://127.0.0.1:8545"
      E.run transport info >>= log
      Assert.assert "" true

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
  blockTxCount <- E.ethGetBlockTransactionCountByHash blockHash
  uncleCount <- E.ethGetUncleCountByBlockNumber defaultBlock
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
Gas price:                  """ <> (show gasPrice) <> """
Accounts:                   """ <> (joinWith ", " $ show <$> accounts) <> """
Most recent block:          """ <> (show recentBlockNumber) <> """
Account balance:            """ <> (show balance) <> """
# of coinbase transactions  """ <> (show accountTxCount) <> """
# of transactions           """ <> (maybe "No transactions found" show blockTxCount) <> """
# of uncles in a block      """ <> (show uncleCount) <> """
"""
