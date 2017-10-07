module Data.BaseChar
  ( OctChar
  , HexChar
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty(..))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (elements)

newtype HexChar = HexChar Char
derive instance newtypeHexChar :: Newtype HexChar _
derive newtype instance eqHexChar :: Eq HexChar
derive newtype instance showHexChar :: Show HexChar
instance arbitraryHexChar :: Arbitrary HexChar where
  arbitrary =
    let chrs = ['a', 'b', 'c', 'd', 'e', 'f',
                '1', '2', '3', '4', '5', '6', '7', '8', '9']
    in elements $ HexChar <$> NonEmpty '0' chrs

newtype OctChar = OctChar Char
derive instance newtypeOctChar :: Newtype OctChar _
derive newtype instance eqOctChar :: Eq OctChar
derive newtype instance showOctChar :: Show OctChar
instance arbitraryOctChar :: Arbitrary OctChar where
  arbitrary = elements $ OctChar <$>
    NonEmpty '0' ['1', '2', '3', '4', '5', '6', '7']
