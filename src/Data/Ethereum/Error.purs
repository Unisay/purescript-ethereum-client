module Data.Ethereum.Error
  ( Error(..)
  , Errors(..)
  , mkErrors
  , squashErrors
  , noteErrors
  , clarify
  , Erroneous
  , type (-!>)
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)

type Erroneous a b = a -> Either Errors b

infix 6 type Erroneous as -!>

newtype Error = Error String
derive instance newtypeError :: Newtype Error _
derive newtype instance eqError :: Eq Error
derive newtype instance showError :: Show Error
derive newtype instance semigroupError :: Semigroup Error

newtype Errors = Errors (NonEmptyList Error)
derive instance newtypeErrors :: Newtype Errors _
derive newtype instance eqErrors :: Eq Errors
derive newtype instance showErrors :: Show Errors
derive newtype instance semigroupErrors :: Semigroup Errors

mkErrors :: String -> Errors
mkErrors = Error >>> singleton >>> Errors

squashErrors :: ∀ a. Either Errors a -> Either String a
squashErrors = lmap show

noteErrors :: ∀ a. String -> Maybe a -> Either Errors a
noteErrors msg mb = lmap mkErrors $ note msg mb

clarify :: ∀ a. String -> Either Errors a -> Either Errors a
clarify s = lmap (unwrap >>> map (append (Error s)) >>> wrap)
