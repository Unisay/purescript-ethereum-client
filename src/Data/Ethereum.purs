module Data.Ethereum
  ( module Bytes
  , module Abi
  , module Network
  , module Sync
  , module Block
  , module Quantity
  , module Address
  , module Signature
  , module Hash
  , TxHash
  , mkTxHash
  , Code(..)
  , Wei(..)
  , Endpoint(..)
  , Contract(..)
  , Transaction(..)
  , Call(..)
  ) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, assoc, encodeJson, extend)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.BigInt as I
import Data.ByteString as B
import Data.Either (Either(Right, Left), either)
import Data.Ethereum.Abi (Abi)
import Data.Ethereum.Bytes as Bytes
import Data.Ethereum.Bytes (Bytes(Bytes))
import Data.Ethereum.Abi as Abi
import Data.Ethereum.Network as Network
import Data.Ethereum.Sync as Sync
import Data.Ethereum.Block as Block
import Data.Ethereum.Quantity as Quantity
import Data.Ethereum.Quantity (Quantity)
import Data.Ethereum.Address as Address
import Data.Ethereum.Address (Address)
import Data.Ethereum.Signature as Signature
import Data.Ethereum.Hash as Hash
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)

type Error = String
type Valid = Either Error



-- | Contract code

newtype Code = Code Bytes

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

newtype TxHash = TxHash Bytes

mkTxHash :: Bytes -> Valid TxHash
mkTxHash (Bytes bs) =
  if (B.length bs /= 32)
  then Left $ "Transaction hash is expected to be exactly 32 bytes "
           <> "but it is "
           <> show (B.length bs)
  else Right $ TxHash (Bytes bs)

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
