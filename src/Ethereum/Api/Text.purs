module Ethereum.Api.Text (fromHex, toHex) where

import Prelude
import Data.ByteString (ByteString, fromString, toString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripPrefix)
import Node.Encoding (Encoding(..))

toHex :: ByteString -> String
toHex bs = "0x" <> toString bs Hex

fromHex :: String -> ByteString
fromHex s = let noPrefix = fromMaybe s $ stripPrefix (Pattern "0x") s
            in fromString noPrefix Hex
