module Test.Unsafe where

import Prelude

import Data.BigInt as I
import Data.ByteString (ByteString, Encoding(..))
import Data.ByteString as B
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Ethereum.Type as E
import Partial.Unsafe (unsafePartial)


class MkUnsafe i o | o -> i where
  mkUnsafe :: i -> o

instance mkUnsafeByteString :: MkUnsafe String ByteString where
  mkUnsafe s = unsafePartial $ fromJust (B.fromString s Hex)

instance mkUnsafeBytes :: MkUnsafe String E.Bytes where
  mkUnsafe = mkUnsafe >>> E.Bytes

instance mkUnsafeBlockHash :: MkUnsafe String E.BlockHash where
  mkUnsafe s = unsafePartial $ fromRight $ E.mkBlockHash $ mkUnsafe s

instance mkUnsafeBlockNumber :: MkUnsafe Int E.BlockNumber where
  mkUnsafe i = unsafePartial $ fromRight $ E.mkBlockNumber $ I.fromInt i

instance mkUnsafeAddress :: MkUnsafe String E.Address where
  mkUnsafe s = unsafePartial $ fromRight $ E.mkAddress $ mkUnsafe s

instance mkUnsafeSignature :: MkUnsafe String E.Signature where
  mkUnsafe s = unsafePartial $ fromRight $ E.mkSignature $ mkUnsafe s

instance mkUnsafeQuantity :: MkUnsafe Int E.Quantity where
  mkUnsafe i = unsafePartial $ fromRight $ E.mkQuantity i

instance mkUnsafeCode :: MkUnsafe String E.Code where
  mkUnsafe = mkUnsafe >>> E.Code
