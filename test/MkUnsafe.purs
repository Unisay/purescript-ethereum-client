module Test.MkUnsafe where

import Prelude

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.BigInt as I
import Data.ByteString (ByteString, Encoding(..))
import Data.ByteString as B
import Data.Either (Either, either)
import Data.Ethereum as E
import Data.Ethereum.Contract (Code(..))
import Data.Ethereum.Bytes (Bytes(..))
import Data.Maybe (Maybe(Nothing, Just))


class MkUnsafe i o | o -> i where
  mkUnsafe :: i -> o

instance mkUnsafeByteString :: MkUnsafe String ByteString where
  mkUnsafe s = unsafeJust (B.fromString s Hex)

instance mkUnsafeBytes :: MkUnsafe String Bytes where
  mkUnsafe = mkUnsafe >>> Bytes

instance mkUnsafeBlockHash :: MkUnsafe String E.BlockHash where
  mkUnsafe = mkUnsafe >>> E.mkBlockHash >>> unsafeRight

instance mkUnsafeBlockNumber :: MkUnsafe Int E.BlockNumber where
  mkUnsafe = I.fromInt >>> E.mkBlockNumber >>> unsafeRight

instance mkUnsafeAddress :: MkUnsafe String E.Address where
  mkUnsafe = mkUnsafe >>> E.mkAddress >>> unsafeRight

instance mkUnsafeSignature :: MkUnsafe String E.Signature where
  mkUnsafe = mkUnsafe >>> E.mkSignature >>> unsafeRight

instance mkUnsafeQuantity :: MkUnsafe Int E.Quantity where
  mkUnsafe = E.mkQuantity >>> unsafeRight

instance mkUnsafeCode :: MkUnsafe String Code where
  mkUnsafe = mkUnsafe >>> Code

instance mkUnsafeTxHash :: MkUnsafe String E.TxHash where
  mkUnsafe = mkUnsafe >>> E.mkTxHash >>> unsafeRight

instance mkUnsafeJson :: MkUnsafe String Json where
  mkUnsafe = jsonParser >>> unsafeRight

unsafeRight :: ∀ a. Either String a -> a
unsafeRight = either handleUnsafeErr id

unsafeJust :: ∀ a. Maybe a -> a
unsafeJust (Just a) = a
unsafeJust Nothing = handleUnsafeErr "Maybe is Nothing"

handleUnsafeErr :: ∀ a. String -> a
handleUnsafeErr = append "Unsafe construction: " >>> unsafeThrow
