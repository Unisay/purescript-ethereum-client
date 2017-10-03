module Data.Ethereum.Signature
  ( Signature
  , mkSignature
  , rsv
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.ByteString as B
import Data.Either (Either(..))
import Data.Ethereum.Bytes (Bytes(..), sliceBytes)
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)

type Error = String
type Valid = Either Error

-- | Ethereum signature used for signing / verification

newtype Signature = Signature Bytes

mkSignature :: Bytes -> Valid Signature
mkSignature (Bytes bs) =
  if (B.length bs /= 32)
  then Left "Signature is expected to be exactly 256 bits"
  else Right $ Signature (Bytes bs)

rsv :: Signature -> { r :: Bytes, s :: Bytes, v :: Bytes }
rsv (Signature bs) = { r : sliceBytes 0 64 bs
                     , s : sliceBytes 64 128 bs
                     , v : sliceBytes 128 130 bs
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
