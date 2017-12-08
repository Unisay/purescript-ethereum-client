module Data.Ethereum.Abi.Type.Property where

import Prelude

import Data.Either (Either(Right))
import Data.Ethereum.Abi.Class (class AbiType, dec, enc)
import Data.String as S
import Test.QuickCheck (Result, (<?>))  

propTypeEncMultiple32b :: ∀ a. AbiType a => Show a => a -> Result
propTypeEncMultiple32b t =
  expected == actual
    <?> "\nExpected:   " <> show expected
    <>  "\nActual:     " <> show actual
    <>  "\nEncoded:    " <> show encoded
    <>  "\nStripped:   " <> show stripped
    <>  "\n# of chars: " <> show chars
    <>  "\nt:          " <> show t
  where
    expected = 0
    actual = bytes `mod` 32
    bytes = chars `div` 2
    chars = S.length stripped
    stripped = S.drop 2 encoded
    encoded = enc t

propTypeEncIsDecodable :: ∀ a. AbiType a => Eq a => Show a => a -> Result
propTypeEncIsDecodable a =
  expected == actual
    <?> "\nExpected:   " <> show expected
    <>  "\nActual:     " <> show actual
    <>  "\nEncoded:    " <> show encoded
  where
    expected = Right a
    actual = dec encoded
    encoded = enc a
