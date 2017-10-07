module Property where


import Prelude

import Data.Char.Unicode (isHexDigit)
import Data.Foldable (all)
import Data.Int (even)
import Data.Newtype (class Newtype, unwrap)
import Data.String (length, null, toCharArray)
import Data.String.Utils (startsWith)
import Ethereum.Hex (stripHexPrefix)
import Test.QuickCheck (Result(..), (<?>))

newtype Results = Results Result

derive instance newtypeResults :: Newtype Results _

instance semigroupResults :: Semigroup Results where
  append (Results (Failed l)) (Results (Failed s)) = Results $ Failed $ l <> "; " <>  s
  append (Results f@(Failed l)) _ = Results $ f
  append _ (Results f@(Failed l)) = Results $ f
  append r _ = r

hasHexPrefix :: String -> Result
hasHexPrefix s = startsWith "0x" s
  <?> ("Has no '0x' prefix: " <> s)

hasOnlyHexDigits :: String -> Result
hasOnlyHexDigits s = (toCharArray >>> all isHexDigit) s
  <?> ("Has not only hexadecimal digits: " <> s)

nonEmpty :: String -> Result
nonEmpty s = (not null) s <?> "Is empty"

hasEvenLength :: String -> Result
hasEvenLength s = even (length s) <?> "Has odd length"

isHexBytesEncoding :: String -> Result
isHexBytesEncoding = hasHexPrefix
                 <&> hasEvenLength
                 <&> stripHexPrefix >>> hasOnlyHexDigits

isHex :: String -> Result
isHex = hasHexPrefix
            <&> stripHexPrefix >>> nonEmpty
            <&> stripHexPrefix >>> hasOnlyHexDigits

and :: ∀ a. (a -> Result) -> (a -> Result) -> a -> Result
and f g = (f >>> Results <> g >>> Results) >>> unwrap

infix 2 and as <&>
