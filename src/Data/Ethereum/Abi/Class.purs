module Data.Ethereum.Abi.Class where

import Data.ByteString (ByteString)
import Data.Ethereum.Bytes (Bytes)
import Prelude (not)

class AbiType a where
  isStatic :: a -> Boolean

isDynamic :: ∀ a. AbiType a => a -> Boolean
isDynamic a = not (isStatic a)

class AbiType a <= Enc a where
  enc :: a -> Bytes

instance abiTypeString :: AbiType String where isStatic _ = false

instance abiTypeByteString :: AbiType ByteString where isStatic _ = false
