module Data.Ethereum.Abi.Type.Property where

import Prelude

import Data.Either (either)
import Data.Ethereum.Abi.Class (class AbiType, enc)
import Data.String as S
import Ethereum (class FromHex, fromHex)
import Test.QuickCheck (Result(..), (<?>), (===))


propTypeEncMultiple32b :: ∀ a. AbiType a => Show a => a -> Result
propTypeEncMultiple32b t =
  let digits = S.length (enc t) - 2
      res = digits `mod` 2 == 0 && digits `mod` 64 == 0
  in res <?> ("propTypeEncMultiple32b did not hold for " <> show t)

propDecodableEnc :: ∀ a.
                    AbiType a =>
                    Eq a =>
                    Show a =>
                    FromHex a =>
                    a -> Result
propDecodableEnc a = either (show >>> Failed) (const Success) do
  decoded <- fromHex (enc a)
  pure $ a === decoded
