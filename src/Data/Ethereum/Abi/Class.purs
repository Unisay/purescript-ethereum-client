module Data.Ethereum.Abi.Class where


class AbiType a where
  enc :: a -> String

instance abiTypeBoolean :: AbiType Boolean where
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
