module Data.Ethereum.Contract
  ( Endpoint(..)
  , Contract(..)
  , Code(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Ethereum.Abi (Abi)
import Data.Ethereum.Address (Address)
import Data.Ethereum.Bytes (Bytes)
import Data.Ethereum.Error (clarify, squashErrors)
import Data.Ethereum.Hash (TxHash)
import Data.Newtype (class Newtype, unwrap)
import Ethereum.Hex (class FromHex, class ToHex, fromHex, toHex)


data Endpoint = Endpoint Address Abi

derive instance eqEndpoint :: Eq Endpoint

instance showEndpoint :: Show Endpoint where
  show (Endpoint addr abi) =
    "Endpoint { " <> show addr
          <> ", " <> show abi
          <> "}"


data Contract = Contract Endpoint TxHash

endpoint :: Contract -> Endpoint
endpoint (Contract ep _) = ep

derive instance eqContract :: Eq Contract

instance showContract :: Show Contract where
  show (Contract (Endpoint addr abi) tx) =
    "Contract { " <> show addr
          <> ", " <> show abi
          <> ", " <> show tx
          <> "}"


-- | Contract code

newtype Code = Code Bytes

derive instance newtypeCode :: Newtype Code _

instance showCode :: Show Code where
  show = toHex >>> append "Code#"

derive instance eqCode :: Eq Code

instance fromHexCode :: FromHex Code where
  fromHex = fromHex >>> map Code

instance toHexCode :: ToHex Code where
  toHex = unwrap >>> toHex

instance decodeJsonCode :: DecodeJson Code where
  decodeJson = decodeJson
    >=> fromHex >>> clarify "Failed to decode Code: " >>> squashErrors
    >>> map Code

instance encodeJsonCode :: EncodeJson Code where
  encodeJson = unwrap >>> encodeJson
