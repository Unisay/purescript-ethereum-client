module Data.Ethereum
  ( module EB
  , module Abi
  , module N
  , SyncStatus(..)
  , BlockNumber
  , mkBlockNumber
  , Quantity
  , mkQuantity
  , Address
  , mkAddress
  , Signature
  , mkSignature
  , rsv
  , Keccak256
  , mkKeccak256
  , BlockHash
  , mkBlockHash
  , TxHash
  , mkTxHash
  , Code(..)
  , Tag(..)
  , Wei(..)
  , Endpoint(..)
  , Contract(..)
  , Transaction(..)
  , Call(..)
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, assoc, encodeJson, extend)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.BigInt as I
import Data.ByteString as B
import Data.Either (Either(Right, Left), either)
import Data.Ethereum.Abi (Abi)
import Data.Ethereum.Bytes as EB
import Data.Ethereum.Abi as Abi
import Data.Ethereum.Network as N
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)

type Error = String
type Valid = Either Error


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

instance fromHexQuantity :: FromHex Quantity where
  fromHex = fromHex >>> map Quantity

instance toHexQuantity :: ToHex Quantity where
  toHex = unwrap >>> toHex

instance decodeJsonQuantity :: DecodeJson Quantity where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Quantity: ")
               >=> mkQuantity

instance encodeJsonQuantity :: EncodeJson Quantity where
  encodeJson = toHex >>> encodeJson


-- | Ethereum address (20 bytes)

newtype Address = Address EB.Bytes

mkAddress :: EB.Bytes -> Valid Address
mkAddress (EB.Bytes bs) =
  if (B.length bs /= 20)
  then Left "Address is expected to be exactly 20 bytes"
  else Right $ Address (EB.Bytes bs)

derive instance newtypeAddress :: Newtype Address _

derive instance eqAddress :: Eq Address

instance showAddress :: Show Address where
  show = toHex >>> append "Address#"

instance fromHexAddress :: FromHex Address where
  fromHex = fromHex >=> mkAddress

instance toHexAddress :: ToHex Address where
  toHex = unwrap >>> toHex

instance decodeAddress :: DecodeJson Address where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Address: ")
               >=> mkAddress

instance encodeAddress :: EncodeJson Address where
  encodeJson = unwrap >>> encodeJson


-- | Ethereum signature used for signing / verification

newtype Signature = Signature EB.Bytes

mkSignature :: EB.Bytes -> Valid Signature
mkSignature (EB.Bytes bs) =
  if (B.isEmpty bs)
  then Left "Signature couldn't be empty"
  else Right $ Signature (EB.Bytes bs)

rsv :: Signature -> { r :: EB.Bytes, s :: EB.Bytes, v :: EB.Bytes }
rsv (Signature bs) = { r : EB.sliceBytes 0 64 bs
                     , s : EB.sliceBytes 64 128 bs
                     , v : EB.sliceBytes 128 130 bs
                     }

derive instance newtypeSignature :: Newtype Signature _

derive instance eqSignature :: Eq Signature

instance showSignature :: Show Signature where
  show = toHex >>> append "Signature#"

instance fromHexSignature :: FromHex Signature where
  fromHex = fromHex >=> mkSignature

instance toHexSignature :: ToHex Signature where
  toHex = unwrap >>> toHex

instance decodeJsonSignature :: DecodeJson Signature where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode HEX as Signature: ")
               >>> map Signature

instance encodeJsonSignature :: EncodeJson Signature where
  encodeJson = unwrap >>> encodeJson


-- | Keccak-256 hash

newtype Keccak256 = Keccak256 EB.Bytes

mkKeccak256 :: EB.Bytes -> Valid Keccak256
mkKeccak256 (EB.Bytes bs) =
  if (B.length bs /= 32)
  then Left "Keccak-256 hash is expected to be exactly 256 bits"
  else Right $ Keccak256 (EB.Bytes bs)

derive instance newtypeKeccak256 :: Newtype Keccak256 _

instance showKeccak256 :: Show Keccak256 where
  show = toHex >>> append "Keccak256#"

derive instance eqKeccak256 :: Eq Keccak256

instance fromHexKeccak256 :: FromHex Keccak256 where
  fromHex = fromHex >=> mkKeccak256

instance toHexKeccak256 :: ToHex Keccak256 where
  toHex = unwrap >>> toHex

instance decodeJsonKeccak256 :: DecodeJson Keccak256 where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Keccak-256 hash: ")
               >=> mkKeccak256

instance encodeJsonKeccak256 :: EncodeJson Keccak256 where
  encodeJson = unwrap >>> encodeJson


-- | Contract code

newtype Code = Code EB.Bytes

derive instance newtypeCode :: Newtype Code _

instance showCode :: Show Code where
  show = toHex >>> append "Code#"

derive instance eqCode :: Eq Code

instance fromHexCode :: FromHex Code where
  fromHex = fromHex >>> map Code

instance toHexCode :: ToHex Code where
  toHex = unwrap >>> toHex

instance decodeJsonCode :: DecodeJson Code where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Code: ")
               >>> map Code

instance encodeJsonCode :: EncodeJson Code where
  encodeJson = unwrap >>> encodeJson


-- | Ethereum block number

newtype BlockNumber = BlockNumber I.BigInt

mkBlockNumber :: I.BigInt -> Valid BlockNumber
mkBlockNumber bi =
  if (bi < zero)
  then Left "Can't make a negative block number"
  else Right $ BlockNumber bi

instance showBlockNumber :: Show BlockNumber where
  show = toHex >>> append "BlockNumber#"

derive instance eqBlockNumber :: Eq BlockNumber

derive instance newtypeBlockNumber :: Newtype BlockNumber _

instance fromHexBlockNumber :: FromHex BlockNumber where
  fromHex = fromHex >>> map BlockNumber

instance toHexBlockNumber :: ToHex BlockNumber where
  toHex = unwrap >>> toHex

instance decodeJsonBlockNumber :: DecodeJson BlockNumber where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode BlockNumber: ")
               >=> mkBlockNumber

instance encodeJsonBlockNumber :: EncodeJson BlockNumber where
  encodeJson = toHex >>> encodeJson


-- | Ethereum block hash (32 bytes)

newtype BlockHash = BlockHash EB.Bytes

mkBlockHash :: EB.Bytes -> Valid BlockHash
mkBlockHash (EB.Bytes bs) =
  if (B.length bs /= 32)
  then Left "Block hash is expected to be exactly 32 bytes"
  else Right $ BlockHash (EB.Bytes bs)

instance showBlockHash :: Show BlockHash where
  show = unwrap >>> toHex >>> append "BlockHash#"

derive instance eqBlockHash :: Eq BlockHash

derive instance newtypeBlockHash :: Newtype BlockHash _

instance toHexBlockHash :: ToHex BlockHash where
  toHex = unwrap >>> toHex

instance fromHexBlockHash :: FromHex BlockHash where
  fromHex = fromHex >>> map BlockHash

instance decodeJsonBlockHash :: DecodeJson BlockHash where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode BlockHash: ")
               >=> mkBlockHash

instance encodeJsonBlockHash :: EncodeJson BlockHash where
  encodeJson = unwrap >>> encodeJson

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

derive instance eqWei :: Eq Wei

derive instance newtypeWei :: Newtype Wei _

instance showWei :: Show Wei where
  show = unwrap >>> I.toString >>> flip append " WEI"

instance fromHex :: FromHex Wei where
  fromHex = fromHex >>> map Wei

instance toHex :: ToHex Wei where
  toHex = unwrap >>> toHex

instance encodeJsonWei :: EncodeJson Wei where
  encodeJson = toHex >>> encodeJson

instance decodeJsonWei :: DecodeJson Wei where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode Wei: ")
               >>> map Wei


-- | Transaction

newtype Transaction = Transaction
  -- | Address the transaction is send from
  { from :: Address
  -- | Address the transaction is directed to (not specified when creating new contract)
  , to :: Maybe Address
  -- | Gas provided for the transaction execution (default: 90000)
  , gas :: Maybe Quantity
  -- | Gas price used for each paid gas (default: To-Be-Determined)
  , gasPrice :: Maybe Quantity
  -- | Value send with this transaction
  , value :: Maybe Quantity
  -- | Compiled code of a contract OR the hash of the invoked method signature and encoded parameters
  , data :: Either Code Abi
  -- | Allows to overwrite own pending transactions that use the same nonce
  , nonce :: Maybe Quantity
  }

derive instance newtypeTransaction :: Newtype Transaction _

derive instance eqTransaction :: Eq Transaction

instance showTransaction :: Show Transaction where
  show = encodeJson >>> stringify

instance encodeJsonTransaction :: EncodeJson Transaction where
  encodeJson (Transaction tx) =
    let dta = either encodeJson encodeJson tx.data
        fs = [ assoc "from" tx.from # Just
             , assoc "to"       <$> tx.to
             , assoc "gas"      <$> tx.gas
             , assoc "gasPrice" <$> tx.gasPrice
             , assoc "value"    <$> tx.value
             , assoc "nonce"    <$> tx.nonce
             , assoc "data" dta # Just
             ]
        fields = catMaybes fs
    in foldl (flip extend) jsonEmptyObject fields


-- | TxHash

newtype TxHash = TxHash EB.Bytes

mkTxHash :: EB.Bytes -> Valid TxHash
mkTxHash (EB.Bytes bs) =
  if (B.length bs /= 32)
  then Left $ "Transaction hash is expected to be exactly 32 bytes "
           <> "but it is "
           <> show (B.length bs)
  else Right $ TxHash (EB.Bytes bs)

derive instance newtypeTxHash :: Newtype TxHash _

instance showTxHash :: Show TxHash where
  show = unwrap >>> toHex >>> append "TxHash#"

derive instance eqTxHash :: Eq TxHash

instance fromHexTxHash :: FromHex TxHash where
  fromHex = fromHex >=> mkTxHash

instance toHexTxHash :: ToHex TxHash where
  toHex = unwrap >>> toHex

instance decodeJsonTxHash :: DecodeJson TxHash where
  decodeJson = decodeJson
               >=> fromHex >>> lmap (append "Failed to decode transaction hash: ")
               >=> mkTxHash

instance encodeJsonTxHash :: EncodeJson TxHash where
  encodeJson = toHex >>> encodeJson


data Endpoint = Endpoint Address Abi

derive instance eqEndpoint :: Eq Endpoint

instance showEndpoint :: Show Endpoint where
  show (Endpoint addr abi) =
    "Endpoint { " <> show addr
          <> ", " <> show abi
          <> "}"


data Contract = Contract Endpoint TxHash

endpoint :: Contract -> Endpoint
endpoint (Contract ep _) = ep

derive instance eqContract :: Eq Contract

instance showContract :: Show Contract where
  show (Contract (Endpoint addr abi) tx) =
    "Contract { " <> show addr
          <> ", " <> show abi
          <> ", " <> show tx
          <> "}"

-- | Call

newtype Call = Call
  -- | Address the transaction is send from
  { from :: Address
  -- | Address the transaction is directed to (not specified when creating new contract)
  , to :: Maybe Address
  -- | Gas provided for the transaction execution (default: 90000)
  , gas :: Maybe Quantity
  -- | Gas price used for each paid gas (default: To-Be-Determined)
  , gasPrice :: Maybe Quantity
  -- | Value send with this transaction
  , value :: Maybe Quantity
  -- | Compiled code of a contract OR the hash of the invoked method signature and encoded parameters
  , data :: Either Code Abi
  }

derive instance newtypeCall :: Newtype Call _

derive instance eqCall :: Eq Call

instance showCall :: Show Call where
  show = encodeJson >>> stringify

instance encodeJsonCall :: EncodeJson Call where
  encodeJson (Call call) =
    let dta = either encodeJson encodeJson call.data
        fs = [ assoc "from" call.from # Just
             , assoc "to"       <$> call.to
             , assoc "gas"      <$> call.gas
             , assoc "gasPrice" <$> call.gasPrice
             , assoc "value"    <$> call.value
             , assoc "data" dta # Just
             ]
        fields = catMaybes fs
    in foldl (flip extend) jsonEmptyObject fields
