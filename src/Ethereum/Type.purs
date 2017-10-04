module Ethereum.Type
  ( Network(..)
  , SyncStatus(..)
  , BlockNumber
  , mkBlockNumber
  , Quantity
  , mkQuantity
  , Address
  , mkAddress
  , Signature
  , mkSignature
  , Keccak256
  , mkKeccak256
  , BlockHash
  , mkBlockHash
  , Code(..)
  , Bytes(..)
  , Tag(..)
  , Wei(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Bifunctor (lmap)
import Data.BigInt as I
import Data.ByteString as B
import Data.Either (Either(Right, Left))
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Text (class FromHex, class ToHex, fromHex, toHex)

type Error = String
type Valid = Either Error

newtype Bytes = Bytes B.ByteString

derive instance newtypeBytes :: Newtype Bytes _

derive instance eqBytes :: Eq Bytes

instance showBytes :: Show Bytes where
  show = unwrap >>> show

instance toHexBytes :: ToHex Bytes where
  toHex = unwrap >>> toHex

instance fromHexBytes :: FromHex Bytes where
  fromHex = fromHex >>> map Bytes

instance decodeJsonBytes :: DecodeJson Bytes where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Bytes")
               >>> map Bytes


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

instance decodeNetwork :: DecodeJson Network where
  decodeJson = decodeJson >>> map parseNetwork where
    parseNetwork :: String -> Network
    parseNetwork "1" = Mainnet
    parseNetwork "2" = Morden
    parseNetwork "3" = Ropsten
    parseNetwork "4" = Rinkeby
    parseNetwork "42" = Kovan
    parseNetwork s = UnknownNet s

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

-- | Quantity: a natural number

newtype Quantity = Quantity Int

mkQuantity :: Int -> Valid Quantity
mkQuantity i = if (i < 0)
               then Left "Can't make a negative Quantity"
               else Right (Quantity i)

derive instance eqQuantity :: Eq Quantity

derive instance newtypeQuantity :: Newtype Quantity _

instance showQuantity :: Show Quantity where
  show = unwrap >>> show

instance decodeJsonQuantity :: DecodeJson Quantity where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Quantity: ")
               >=> mkQuantity

instance fromHexQuantity :: FromHex Quantity where
  fromHex = fromHex >>> map Quantity

instance toHexQuantity :: ToHex Quantity where
  toHex = unwrap >>> toHex


-- | Ethereum address (20 bytes)

newtype Address = Address Bytes

mkAddress :: Bytes -> Valid Address
mkAddress (Bytes bs) =
  if (B.length bs /= 20)
  then Left "Address is expected to be exactly 20 bytes"
  else Right $ Address (Bytes bs)

instance decodeAddress :: DecodeJson Address where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Address")
               >=> mkAddress

instance showAddress :: Show Address where
  show (Address bs) = toHex bs

derive instance newtypeAddress :: Newtype Address _

instance fromHexAddress :: FromHex Address where
  fromHex = fromHex >=> mkAddress

instance toHexAddress :: ToHex Address where
  toHex = unwrap >>> toHex >>> append "Address#"


-- | Ethereum signature used for signing / verification

newtype Signature = Signature Bytes

mkSignature :: Bytes -> Valid Signature
mkSignature (Bytes bs) =
  if (B.isEmpty bs)
  then Left "Signature couldn't be empty"
  else Right $ Signature (Bytes bs)

derive instance newtypeSignature :: Newtype Signature _

derive instance eqSignature :: Eq Signature

instance decodeSignature :: DecodeJson Signature where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode HEX as Signature")
               >>> map Signature

instance showSignature :: Show Signature where
  show = unwrap >>> toHex >>> append "Signature#"

instance fromHexSignature :: FromHex Signature where
  fromHex = fromHex >=> mkSignature

instance toHexSignature :: ToHex Signature where
  toHex = unwrap >>> toHex


-- | Keccak-256 hash

newtype Keccak256 = Keccak256 Bytes

mkKeccak256 :: Bytes -> Valid Keccak256
mkKeccak256 (Bytes bs) =
  if (B.length bs /= 256)
  then Left "Keccak-256 hash is expected to be exactly 256 bytes"
  else Right $ Keccak256 (Bytes bs)

derive instance newtypeKeccak256 :: Newtype Keccak256 _

instance showKeccak256 :: Show Keccak256 where
  show = unwrap >>> toHex >>> append "Keccak256#"

derive instance eqKeccak256 :: Eq Keccak256

instance fromHexKeccak256 :: FromHex Keccak256 where
  fromHex = fromHex >=> mkKeccak256

instance toHexKeccak256 :: ToHex Keccak256 where
  toHex = unwrap >>> toHex

instance decodeJsonKeccak256 :: DecodeJson Keccak256 where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Keccak-256 hash: ")
               >=> mkKeccak256


-- | Contract code

newtype Code = Code Bytes

derive instance newtypeCode :: Newtype Code _

instance showCode :: Show Code where
  show = unwrap >>> toHex >>> append "Code#"

derive instance eqCode :: Eq Code

instance fromHexCode :: FromHex Code where
  fromHex = fromHex >>> map Code

instance toHexCode :: ToHex Code where
  toHex = unwrap >>> toHex

instance decodeJsonCode :: DecodeJson Code where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Code: ")
               >>> map Code


-- | Ethereum block number

newtype BlockNumber = BlockNumber I.BigInt

mkBlockNumber :: I.BigInt -> Valid BlockNumber
mkBlockNumber bi =
  if (bi < zero)
  then Left "Can't make a negative block number"
  else Right $ BlockNumber bi

instance decodeBlockNumber :: DecodeJson BlockNumber where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode BlockNumber: ")
               >=> mkBlockNumber

instance showBlockNumber :: Show BlockNumber where
  show = unwrap >>> toHex >>> append "BlockNumber#"

derive instance eqBlockNumber :: Eq BlockNumber

derive instance newtypeBlockNumber :: Newtype BlockNumber _

instance fromHexBlockNumber :: FromHex BlockNumber where
  fromHex = fromHex >>> map BlockNumber

instance toHexBlockNumber :: ToHex BlockNumber where
  toHex = unwrap >>> toHex


-- | Ethereum block hash (32 bytes)

newtype BlockHash = BlockHash Bytes

mkBlockHash :: Bytes -> Valid BlockHash
mkBlockHash (Bytes bs) =
  if (B.length bs /= 32)
  then Left "Block hash is expected to be exactly 32 bytes"
  else Right $ BlockHash (Bytes bs)

instance decodeBlockHash :: DecodeJson BlockHash where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode BlockHash")
               >=> mkBlockHash

instance showBlockHash :: Show BlockHash where
  show = unwrap >>> toHex >>> append "BlockHash#"

derive instance eqBlockHash :: Eq BlockHash

derive instance newtypeBlockHash :: Newtype BlockHash _

instance toHexBlockHash :: ToHex BlockHash where
  toHex = unwrap >>> toHex

instance fromHexBlockHash :: FromHex BlockHash where
  fromHex = fromHex >>> map BlockHash


-- | Ethereum block tag

data Tag = Earliest -- the earliest/genesis block
         | Latest   -- the latest mined block
         | Pending  -- the pending state/transactions

derive instance eqTag :: Eq Tag

instance showTag :: Show Tag where
  show Earliest = "earlises"
  show Latest   = "latest"
  show Pending  = "pending"


-- | Ether amount in WEI

newtype Wei = Wei I.BigInt

instance decodeWei :: DecodeJson Wei where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Wei: ")
               >>> map Wei

derive instance eqWei :: Eq Wei

derive instance newtypeWei :: Newtype Wei _

instance showWei :: Show Wei where
  show = unwrap >>> I.toString >>> flip append " WEI"

instance fromHex :: FromHex Wei where
  fromHex = fromHex >>> map Wei

instance toHex :: ToHex Wei where
  toHex = unwrap >>> toHex
