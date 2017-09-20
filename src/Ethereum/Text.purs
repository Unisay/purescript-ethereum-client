module Ethereum.Text (fromHex, toHex, fromHexQuantity) where

import Prelude

import Data.ByteString (ByteString, fromString, toString)
import Data.Int (fromStringAs, hexadecimal)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), stripPrefix, toUpper)
import Node.Encoding (Encoding(..))

toHex :: ByteString -> String
toHex bs = "0x" <> toUpper (toString bs Hex)

fromHex :: String -> ByteString
fromHex s = let noPrefix = fromMaybe s $ stripPrefix (Pattern "0x") s
            in fromString noPrefix Hex

fromHexQuantity :: String -> Maybe Int
fromHexQuantity s = let noPrefix = fromMaybe s $ stripPrefix (Pattern "0x") s
                    in fromStringAs hexadecimal noPrefix
