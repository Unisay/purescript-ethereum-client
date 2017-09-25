module Ethereum.Text
  ( class ToHex
  , class FromHex
  , fromHex
  , toHex
  ) where

import Prelude

import Data.BigInt as BI
import Data.ByteString (Encoding(..))
import Data.ByteString as BS
import Data.Int (fromNumber, hexadecimal, odd, toStringAs)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), length, stripPrefix)

class ToHex a where
  toHex :: a -> String

instance byteStringToHex :: ToHex BS.ByteString where
  toHex bs = "0x" <> BS.toString bs Hex

instance bigIntToHex :: ToHex BI.BigInt where
  toHex bi = "0x" <> BI.toBase 16 bi

instance intToHex :: ToHex Int where
  toHex = toStringAs hexadecimal


class FromHex a where
  fromHex :: String -> Maybe a

instance fromHexByteString :: FromHex BS.ByteString where
  fromHex s = let noPrefix = fromMaybe s $ stripPrefix (Pattern "0x") s
                  padded = if odd $ length noPrefix then "0" <> noPrefix else noPrefix
              in BS.fromString padded Hex

instance fromHexBigInt :: FromHex BI.BigInt where
  fromHex s = let noPrefix = fromMaybe s $ stripPrefix (Pattern "0x") s
              in BI.fromBase 16 noPrefix

instance fromHexInt :: FromHex Int where
  -- | Loses precision for numbers outside the range [-9007199254740992, 9007199254740992].
  fromHex s = (fromHex s) <#> BI.toNumber >>= fromNumber
