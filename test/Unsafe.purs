module Test.Unsafe where

import Prelude
import Data.ByteString (Encoding(..))
import Data.ByteString as BS
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)


unsafeByteString :: String -> BS.ByteString
unsafeByteString s = unsafePartial $ fromJust (BS.fromString s Hex)
