module Ethereum.Text (
    fromHex
  , toHex
  , fromHexQuantity
  , fromHexQuantity'
  ) where

import Prelude
import Data.BigInt (BigInt, fromBase, toNumber)
import Data.ByteString (ByteString, fromString, toString)
import Data.Int (fromNumber, odd)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), length, stripPrefix)
import Node.Encoding (Encoding(..))

toHex :: ByteString -> String
toHex bs = "0x" <> toString bs Hex

fromHex :: String -> Maybe ByteString
fromHex s = let noPrefix = fromMaybe s $ stripPrefix (Pattern "0x") s
                padded = if odd $ length noPrefix then "0" <> noPrefix else noPrefix
            in fromString padded Hex

fromHexQuantity :: String -> Maybe BigInt
fromHexQuantity s = let noPrefix = fromMaybe s $ stripPrefix (Pattern "0x") s
                    in fromBase 16 noPrefix

-- | Loses precision for numbers outside the range [-9007199254740992, 9007199254740992].
fromHexQuantity' :: String -> Maybe Int
fromHexQuantity' s = (fromHexQuantity s) <#> toNumber >>= fromNumber
