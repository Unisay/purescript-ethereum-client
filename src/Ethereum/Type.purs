module Ethereum.Type where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.BigInt (BigInt, toString)
import Data.ByteString (ByteString, isEmpty)
import Data.Either (Either(..), note)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (length)
import Ethereum.Text (class FromHex, class ToHex, fromHex, toHex)

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

newtype Quantity = Quantity Int
derive instance eqQuantity :: Eq Quantity
derive instance newtypeQuantity :: Newtype Quantity _
instance showQuantity :: Show Quantity where show = unwrap >>> show
instance toHexQuantity :: ToHex Quantity where toHex = unwrap >>> toHex


newtype Address = Address ByteString

instance decodeAddress :: DecodeJson Address where
  decodeJson json = do
    hex <- decodeJson json
    when (length hex /= (20 * 2 + 2)) $ Left "Address is expected to contain exactly 20 bytes"
    bs <- note "Failed to decode HEX string" $ fromHex hex
    when (isEmpty bs) $ Left "Empty address"
    pure $ Address bs

instance showAddress :: Show Address where
  show (Address bs) = toHex bs

derive instance newtypeAddress :: Newtype Address _
instance toHexAddress :: ToHex Address where toHex = unwrap >>> toHex


newtype BlockNumber = BlockNumber BigInt

instance decodeBlockNumber :: DecodeJson BlockNumber where
  decodeJson json = do
    hex <- decodeJson json
    bi <- note "Failed to decode HEX number" $ fromHex hex
    pure $ BlockNumber bi

instance showBlockNumber :: Show BlockNumber where
  show = unwrap >>> toHex >>> append "BlockNumber#"

derive instance eqBlockNumber :: Eq BlockNumber
derive instance newtypeBlockNumber :: Newtype BlockNumber _
instance toHexBlockNumber :: ToHex BlockNumber where toHex = unwrap >>> toHex
instance fromHexBlockNumber :: FromHex BlockNumber where
  fromHex hex = BlockNumber <$> fromHex hex

-- | 32 Bytes - hash of a block
newtype BlockHash = BlockHash ByteString

instance decodeBlockHash :: DecodeJson BlockHash where
  decodeJson json = do
    hex <- decodeJson json
    when (length hex /= (32 * 2 + 2)) $ Left "Block hash is expected to contain exactly 32 bytes"
    bs <- note "Failed to decode HEX string" $ fromHex hex
    pure $ BlockHash bs

instance showBlockHash :: Show BlockHash where
  show = unwrap >>> toHex >>> append "BlockHash#"

derive instance eqBlockHash :: Eq BlockHash
derive instance newtypeBlockHash :: Newtype BlockHash _
instance toHexBlockHash :: ToHex BlockHash where toHex = unwrap >>> toHex
instance fromHexBlockHash :: FromHex BlockHash where
  fromHex hex = BlockHash <$> fromHex hex


data Tag = Earliest -- the earliest/genesis block
         | Latest   -- the latest mined block
         | Pending  -- the pending state/transactions

derive instance eqTag :: Eq Tag

instance showTag :: Show Tag where
  show Earliest = "earlises"
  show Latest   = "latest"
  show Pending  = "pending"


newtype Wei = Wei BigInt

instance decodeWei :: DecodeJson Wei where
  decodeJson json = do
    h <- decodeJson json
    i <- maybe (Left "Can't parse Wei amount") Right $ fromHex h
    pure $ Wei i

derive instance eqWei :: Eq Wei
derive instance newtypeWei :: Newtype Wei _
instance showWei :: Show Wei where show = unwrap >>> toString >>> flip append " WEI"
instance toHex :: ToHex Wei where toHex = unwrap >>> toHex
