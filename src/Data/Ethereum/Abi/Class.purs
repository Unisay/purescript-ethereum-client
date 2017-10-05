module Data.Ethereum.Abi.Class where

import Prelude

class AbiType a where
  isStatic :: a -> Boolean
  enc :: a -> String

isDynamic :: ∀ a. AbiType a => a -> Boolean
isDynamic a = not (isStatic a)

instance abiTypeBoolean :: AbiType Boolean where
  isStatic _ = true
  enc true  = "0x0000000000000000000000000000000000000000000000000000000000000001"
  enc false = "0x0000000000000000000000000000000000000000000000000000000000000000"

--
-- instance abiTypeString :: AbiType String where
--   isStatic _ = false
--   encode = ?x
--
-- instance abiTypeByteString :: AbiType ByteString where
--   isStatic _ = false
--   encode = ?x
