module Ethereum.Type where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.BigInt (BigInt, toString)
import Data.ByteString (ByteString, isEmpty)
import Data.Either (Either(..), note)
import Data.Maybe (maybe)
import Ethereum.Text (fromHex, fromHexQuantity, toHex)

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


newtype SyncStatus = SyncStatus {
  startingBlock :: Block,
  currentBlock  :: Block,
  highestBlock  :: Block
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

newtype Quantity = Quantity Int

instance decodeQuantity :: DecodeJson Quantity where
  decodeJson json = Quantity <$> decodeJson json


newtype Address = Address ByteString

instance decodeAddress :: DecodeJson Address where
  decodeJson json = do
    s <- decodeJson json
    bs <- note "Failed to decode HEX string" $ fromHex s
    when (isEmpty bs) $ Left "Empty address"
    pure $ Address bs

instance showAddress :: Show Address where
  show (Address bs) = toHex bs

newtype Block = Block BigInt

instance decodeBlock :: DecodeJson Block where
  decodeJson json = do
    s <- decodeJson json
    i <- note "Failed to decode HEX string" $ fromHexQuantity s
    pure $ Block i

instance showBlock :: Show Block where
  show (Block i) = "Block (" <> (toString i) <> ")"

derive instance eqBlock :: Eq Block

newtype Wei = Wei BigInt

instance decodeWei :: DecodeJson Wei where
  decodeJson json = do
    h <- decodeJson json
    i <- maybe (Left "Can't parse Wei amount") Right $ fromHexQuantity h
    pure $ Wei i

instance showWei :: Show Wei where
  show (Wei i) = toString i
