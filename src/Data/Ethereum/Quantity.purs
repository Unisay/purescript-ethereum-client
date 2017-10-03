module Data.Ethereum.Quantity
  ( Quantity
  , mkQuantity
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)

type Error = String
type Valid = Either Error

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
