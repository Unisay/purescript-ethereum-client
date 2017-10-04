module Data.Ethereum.Abi.Class where

import Prelude (not)

class AbiType a where
  isStatic :: a -> Boolean
  enc :: a -> String

isDynamic :: ∀ a. AbiType a => a -> Boolean
isDynamic a = not (isStatic a)
--
-- instance abiTypeString :: AbiType String where
--   isStatic _ = false
--   encode = ?x
--
-- instance abiTypeByteString :: AbiType ByteString where
--   isStatic _ = false
--   encode = ?x
