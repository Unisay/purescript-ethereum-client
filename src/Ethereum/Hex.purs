module Ethereum.Hex
  ( class ToHex
  , class FromHex
  , fromHex
  , toHex
  ) where

import Prelude

import Data.BigInt as BI
import Data.ByteString (Encoding(..))
import Data.ByteString as BS
import Data.Either (Either, note)
import Data.Int (fromNumber, odd)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), length, stripPrefix)

-- | https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding

class ToHex a where
  toHex :: a -> String

{-
  When encoding UNFORMATTED DATA (byte arrays, account addresses, hashes, bytecode arrays):
  encode as hex, prefix with "0x", two hex digits per byte.
  Examples:
    0x41 (size 1, "A")
    0x004200 (size 3, "\0B\0")
    0x (size 0, "")
    WRONG: 0xf0f0f (must be even number of digits)
    WRONG: 004200 (must be prefixed 0x)
-}
instance byteStringToHex :: ToHex BS.ByteString where
  toHex bs = "0x" <> BS.toString bs Hex

{-
  When encoding QUANTITIES (integers, numbers):
  encode as hex, prefix with "0x", the most compact representation
  (slight exception: zero should be represented as "0x0").
  Examples:
    0x41 (65 in decimal)
    0x400 (1024 in decimal)
    WRONG: 0x (should always have at least one digit - zero is "0x0")
    WRONG: 0x0400 (no leading zeroes allowed)
    WRONG: ff (must be prefixed 0x)
-}
instance bigIntToHex :: ToHex BI.BigInt where
  toHex bi = "0x" <> quantitified where
    quantitified = fromMaybe hex $ stripPrefix (Pattern "0") hex
    hex = BI.toBase 16 bi

instance intToHex :: ToHex Int where
  toHex =  BI.fromInt >>> toHex

class FromHex a where
  fromHex :: String -> Either String a

instance fromHexByteString :: FromHex BS.ByteString where
  fromHex s = let noPrefix = fromMaybe s $ stripPrefix (Pattern "0x") s
                  padded = if odd $ length noPrefix then "0" <> noPrefix else noPrefix
              in note "Failed to read HEX as ByteString" $ BS.fromString padded Hex

instance fromHexBigInt :: FromHex BI.BigInt where
  fromHex s = let noPrefix = fromMaybe s $ stripPrefix (Pattern "0x") s
              in note "Failed to read HEX as BigInt" $ BI.fromBase 16 noPrefix

instance fromHexInt :: FromHex Int where
  -- | Loses precision for numbers outside the range [-9007199254740992, 9007199254740992].
  fromHex s = (fromHex s) <#> BI.toNumber >>= fromNumber >>> note "Failed to read HEX as Int"
