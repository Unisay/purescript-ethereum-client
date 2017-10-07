module Data.Ethereum.Address
  ( Address
  , mkAddress
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.ByteString as B
import Data.Either (Either(..))
import Data.Ethereum.Bytes (Bytes(..))
import Data.Ethereum.Error (type (-!>), clarify, mkErrors, squashErrors)
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)


-- | Ethereum address (20 bytes)

newtype Address = Address Bytes

mkAddress :: Bytes -!> Address
mkAddress (Bytes bs) =
  if (B.length bs /= 20)
  then Left $ mkErrors "Address is expected to be exactly 20 bytes"
  else Right $ Address (Bytes bs)

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
    >=> fromHex >>> clarify  "Failed to decode Address: " >>> squashErrors
    >=> mkAddress >>> squashErrors

instance encodeAddress :: EncodeJson Address where
  encodeJson = unwrap >>> encodeJson
