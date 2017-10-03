module Data.Ethereum.Sync
  ( SyncStatus(..)
  ) where


import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Ethereum.Block (BlockNumber)


newtype SyncStatus = SyncStatus {
  startingBlock :: BlockNumber,
  currentBlock  :: BlockNumber,
  highestBlock  :: BlockNumber
}

derive instance eqSyncStatus :: Eq SyncStatus

instance decodeSyncStatus :: DecodeJson SyncStatus where
  decodeJson json = do
    obj <- decodeJson json
    startingBlock <- obj .? "startingBlock"
    currentBlock <- obj .? "currentBlock"
    highestBlock <- obj .? "highestBlock"
    pure $ SyncStatus { startingBlock: startingBlock
                      , currentBlock:  currentBlock
                      , highestBlock:  highestBlock
                      }

instance showSyncStatus :: Show SyncStatus where
  show (SyncStatus ss) = "startingBlock = '" <> (show ss.startingBlock) <> "', "
                      <> "currentBlock = '"  <> (show ss.currentBlock)  <> "', "
                      <> "highestBlock = '"  <> (show ss.highestBlock)  <> "' "
