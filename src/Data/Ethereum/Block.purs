module Data.Ethereum.Block
  ( BlockNumber(..)
  , mkBlockNumber
  , BlockHash
  , mkBlockHash
  , Tag(..)
  ) where


import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.BigInt as I
import Data.ByteString as B
import Data.Either (Either(..))
import Data.Ethereum.Bytes (Bytes(..))
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class ToHex, class FromHex, toHex, fromHex)

type Error = String
type Valid = Either Error

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

newtype BlockHash = BlockHash Bytes

mkBlockHash :: Bytes -> Valid BlockHash
mkBlockHash (Bytes bs) =
  if (B.length bs /= 32)
  then Left "Block hash is expected to be exactly 32 bytes"
  else Right $ BlockHash (Bytes bs)

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
