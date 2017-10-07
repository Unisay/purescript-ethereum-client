module Data.Ethereum.Quantity
  ( Quantity
  , mkQuantity
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.Ethereum.Error (type (-!>), clarify, mkErrors, squashErrors)
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)

-- | Quantity: a natural number

newtype Quantity = Quantity BigInt

mkQuantity :: BigInt -!> Quantity
mkQuantity i =
  if (i < zero)
  then Left $ mkErrors "Can't make a negative Quantity"
  else Right (Quantity i)

derive instance eqQuantity :: Eq Quantity

derive instance newtypeQuantity :: Newtype Quantity _

instance showQuantity :: Show Quantity where
  show = unwrap >>> show

instance fromHex :: FromHex Quantity where
  fromHex = fromHex >>> map Quantity

instance toHex :: ToHex Quantity where
  toHex = unwrap >>> toHex

instance decodeJsonQuantity :: DecodeJson Quantity where
  decodeJson = decodeJson
    >=> fromHex >>> clarify "Failed to decode Quantity: " >>> squashErrors
    >=> mkQuantity >>> squashErrors

instance encodeJsonQuantity :: EncodeJson Quantity where
  encodeJson = toHex >>> encodeJson
