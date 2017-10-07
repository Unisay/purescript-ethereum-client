module Data.Ethereum.Bytes
  ( Bytes(..)
  , mkBytes
  , sliceBytes
) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.ByteString as B
import Data.Ethereum.Error (clarify, squashErrors)
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)
import Node.Buffer.Unsafe (slice)


newtype Bytes = Bytes B.ByteString

mkBytes :: String -> Bytes
mkBytes = B.toUTF8 >>> Bytes

sliceBytes :: Int -> Int -> Bytes -> Bytes
sliceBytes from to (Bytes bs) =
  Bytes $ B.unsafeFreeze (slice from to (B.unsafeThaw bs))

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
    >=> fromHex >>> clarify "Failed to decode Bytes: " >>> squashErrors
    >>> map Bytes

instance encodeJsonBytes :: EncodeJson Bytes where
  encodeJson = unwrap >>> toHex >>> encodeJson
