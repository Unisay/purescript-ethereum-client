module Ethereum.Hex
  ( class ToHex
  , class FromHex
  , fromHex
  , toHex
  , stripHexPrefix
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.ByteString (ByteString, Encoding(..))
import Data.ByteString as BS
import Data.Ethereum.Error (type (-!>), noteErrors)
import Data.Int (fromNumber, odd)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), dropWhile, length, null, stripPrefix)

{-
  https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding

  When encoding UNFORMATTED DATA (byte arrays, account addresses, hashes, bytecode arrays):
  encode as hex, prefix with "0x", two hex digits per byte.

  Examples:
    0x41 (size 1, "A")
    0x004200 (size 3, "\0B\0")
    0x (size 0, "")
    WRONG: 0xf0f0f (must be even number of digits)
    WRONG: 004200 (must be prefixed 0x)

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

stripHexPrefix :: String -> String
stripHexPrefix s = fromMaybe s $ stripPrefix (Pattern "0x") s

class ToHex a where
  toHex :: a -> String

instance byteStringToHex :: ToHex ByteString where
  toHex bs = "0x" <> BS.toString bs Hex

instance toHexBigInt :: ToHex BigInt where
  toHex = prefix <<< checkEmpty <<< trim <<< makeRaw
    where
      prefix = append "0x"
      checkEmpty = \s -> if null s then "0" else s
      trim = dropWhile (_ == '0')
      makeRaw = BI.abs >>> BI.toBase 16



class FromHex a where
  fromHex :: String -!> a

instance fromHexByteString :: FromHex ByteString where
  fromHex s =
    let noPrefix = stripHexPrefix s
        padded = if odd $ length noPrefix then "0" <> noPrefix else noPrefix
    in noteErrors "Failed to read hexadecimal string as ByteString" $ BS.fromString padded Hex

instance fromHexBigInt :: FromHex BigInt where
  fromHex s =
    let noPrefix = stripHexPrefix s
    in noteErrors "Failed to read HEX as BigInt" $ BI.fromBase 16 noPrefix

instance fromHexInt :: FromHex Int where
  -- | Loses precision for numbers outside the range [-9007199254740992, 9007199254740992].
  fromHex s = (fromHex s)
                  <#> BI.toNumber >>= fromNumber >>> noteErrors "Failed to read HEX as Int"
