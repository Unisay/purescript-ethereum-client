module Data.Ethereum.Network
  ( Network (..)
  ) where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson)

data Network = Mainnet
             | Morden
             | Ropsten
             | Rinkeby
             | Kovan
             | UnknownNet String

derive instance eqNetwork :: Eq Network

instance showNetwork :: Show Network where
  show Mainnet = "Ethereum Mainnet"
  show Morden = "Morden Testnet"
  show Ropsten = "Ropsten Testnet"
  show Rinkeby = "Rinkeby Testnet"
  show Kovan = "Kovan Testnet"
  show (UnknownNet s) = "Unknown network: " <> s

instance decodeNetwork :: DecodeJson Network where
  decodeJson = decodeJson >>> map parseNetwork where
    parseNetwork :: String -> Network
    parseNetwork "1" = Mainnet
    parseNetwork "2" = Morden
    parseNetwork "3" = Ropsten
    parseNetwork "4" = Rinkeby
    parseNetwork "42" = Kovan
    parseNetwork s = UnknownNet s
