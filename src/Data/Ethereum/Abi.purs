module Data.Ethereum.Abi
  ( Abi(..)
  , mkAbi
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Ethereum.Bytes (Bytes, mkBytes)
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class ToHex, toHex)


-- | Application Binary Interface
-- | https://solidity.readthedocs.io/en/develop/abi-spec.html
-- | TODO: implement Abi algebra

newtype Abi = Abi Bytes

mkAbi :: Json -> Abi
mkAbi = stringify >>> mkBytes >>> Abi

derive instance newtypeAbi :: Newtype Abi _

derive instance eqAbi :: Eq Abi

instance showAbi :: Show Abi where
  show = unwrap >>> show >>> append "ABI#"

instance toHexAbi :: ToHex Abi where
  toHex = unwrap >>> toHex

instance encodeJson :: EncodeJson Abi where
  encodeJson = unwrap >>> encodeJson
