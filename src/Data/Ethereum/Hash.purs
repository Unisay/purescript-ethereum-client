module Data.Ethereum.Hash
  ( Keccak256
  , mkKeccak256
  ) where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.ByteString as B
import Data.Either (Either(..))
import Data.Ethereum.Bytes (Bytes(Bytes))
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)


type Error = String
type Valid = Either Error

-- | Keccak-256 hash

newtype Keccak256 = Keccak256 Bytes

mkKeccak256 :: Bytes -> Valid Keccak256
mkKeccak256 (Bytes bs) =
  if (B.length bs /= 32)
  then Left "Keccak-256 hash is expected to be exactly 256 bits"
  else Right $ Keccak256 (Bytes bs)

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
