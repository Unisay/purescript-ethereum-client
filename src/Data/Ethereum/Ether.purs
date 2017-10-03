module Data.Ethereum.Ether
  ( Wei(..)
  ) where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.BigInt as I
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)


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
