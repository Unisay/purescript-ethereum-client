module Data.Ethereum.Transaction
  ( Transaction(..)
  , Call(..)
) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject, stringify)
import Data.Argonaut.Encode (class EncodeJson, assoc, encodeJson, extend)
import Data.Array (catMaybes)
import Data.Either (Either, either)
import Data.Ethereum.Abi (Abi)
import Data.Ethereum.Address (Address)
import Data.Ethereum.Contract (Code)
import Data.Ethereum.Quantity (Quantity)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)


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
