module Data.Ethereum.Abi.Type
  ( Address
  , Bool
  , mkBool
  , Bytes
  , mkBytes
  , UnsignedInt
  , mkUnsignedInt
  , SignedInt
  , mkSignedInt
  , UnsignedFixed
  , mkUnsignedFixed
  , SignedFixed
  , mkSignedFixed
  , FixedLenArray
  , mkFixedLenArray
  , VarLenArray
  , mkVarLenArray
  , class Dividend8
  , class From1to32
  , class From1to80
  ) where

import Prelude
import Data.Array as A
import Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Ethereum.Abi.Class (class AbiType)
import Data.Int (pow)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class LtEq, class Mod, class Pos, type (:*), D0, D1, D19, D2, D24, D3, D5, D6, D8, D80, toInt)


newtype Address = Address (UnsignedInt (D1 :* D6 :* D0))

-- instance abiTypeAddress :: AbiType Address where
--   isStatic _ = true
--   encode (Address uint) = ?x


newtype Bool = Bool (UnsignedInt D8)
-- instance abiTypeBool :: AbiType Bool where isStatic _ = true

mkBool :: Boolean -> Bool
mkBool false = Bool (UnsignedInt 0)
mkBool true = Bool (UnsignedInt 1)

class (Pos m, LtEq m (D2 :* D5 :* D6), Mod m D8 D0) <= Dividend8 m
instance dividend8TypeLevel :: (Pos m, LtEq m (D2 :* D5 :* D6), Mod m D8 D0) => Dividend8 m

-- | uint<M>: unsigned integer type of M bits, 0 < M <= 256, M % 8 == 0
newtype UnsignedInt m = UnsignedInt Int
-- instance abiTypeUnsignedInt :: Dividend8 m => AbiType (UnsignedInt m) where isStatic _ = true

-- | Unsigned n-bit integer: [0, 2^n)
mkUnsignedInt :: ∀ m. Dividend8 m => m -> Int -> Maybe (UnsignedInt m)
mkUnsignedInt m i
  | i >= 0 && i < 2 `pow` (toInt m) = Just $ UnsignedInt i
  | otherwise = Nothing


-- | int<M>: two’s complement signed integer type of M bits, 0 < M <= 256, M % 8 == 0
newtype SignedInt m  = SignedInt Int
-- instance abiTypeSignedInt :: Dividend8 m => AbiType (SignedInt m) where isStatic _ = true

-- | Signed n-bit integer: [−2^(n−1), 2^(n−1))
mkSignedInt :: ∀ m. Dividend8 m => m -> Int -> Maybe (SignedInt m)
mkSignedInt m i = let p = 2 `pow` (toInt m - 1)
                  in if (-p <= i && i < p)
                     then Just $ SignedInt i
                     else Nothing


-- | bytes<M>: binary type of M bytes, 0 < M <= 32
class (Pos m, LtEq m (D3 :* D2)) <= From1to32 m
instance from1to32TypeLevel :: (Pos m, LtEq m (D3 :* D2)) => From1to32 m

newtype Bytes m = Bytes ByteString
-- instance abiTypeBytes :: From1to32 m =>
--                           AbiType (Bytes m) where
--                           isStatic _ = true

mkBytes :: ∀ m. From1to32 m => m -> ByteString -> Maybe (Bytes m)
mkBytes m bs
  | toInt m == B.length bs = Just $ Bytes bs
  | otherwise = Nothing

newtype Func = Function (Bytes D24)
-- instance abiTypeFunc :: AbiType Func where isStatic _ = true


-- | fixed<M>x<N>: signed fixed-point decimal number of M bits,
-- | 8 <= M <= 256, M % 8 ==0, and 0 < N <= 80,
-- | which denotes the value v as v / (10 ** N)
class (Pos m, LtEq m D80) <= From1to80 m

data SignedFixed m n = SignedFixed (SignedInt m) (UnsignedInt n)
-- instance abiTypeSignedFixed :: (Dividend8 m, From1to80 n) =>
--                           AbiType (SignedFixed m n) where
--                           isStatic _ = true

mkSignedFixed :: ∀ m n.
                 Dividend8 m =>
                 From1to80 n =>
                 SignedInt m -> UnsignedInt n -> SignedFixed m n
mkSignedFixed = SignedFixed


data UnsignedFixed m n = UnsignedFixed (UnsignedInt m) (UnsignedInt n)
-- instance abiTypeUnsignedFixed :: (Dividend8 m, From1to80 n) =>
--                           AbiType (UnsignedFixed m n) where
--                           isStatic _ = true

mkUnsignedFixed:: ∀ m n.
           Dividend8 m =>
           From1to80 n =>
           UnsignedInt m -> UnsignedInt n -> UnsignedFixed m n
mkUnsignedFixed = UnsignedFixed

type Fixed = SignedFixed (D1 :* D2 :* D8) D19
type UFixed = UnsignedFixed (D1 :* D2 :* D8) D19

-- | <type>[M]: a fixed-length array of M elements, M > 0, of the given type
newtype FixedLenArray m a = FixedLenArray (Array a)
-- instance abiTypeFixedLenArray :: AbiType (FixedLenArray m a) where
--   isStatic _ = true

mkFixedLenArray :: ∀ m a.
                   Pos m =>
                   AbiType a =>
                   m -> Array a -> Maybe (FixedLenArray m a)
mkFixedLenArray m as
  | A.length as == toInt m = Just $ FixedLenArray as
  | otherwise = Nothing

-- | <type>[]: a variable-length array of elements of the given type
newtype VarLenArray a = VarLenArray (Array a)
-- instance abiTypeVarLenArray :: AbiType a => AbiType (VarLenArray a) where
--   isStatic _ = false

mkVarLenArray :: ∀ a. AbiType a => Array a -> (VarLenArray a)
mkVarLenArray = VarLenArray

{-

  Other dynamic Abi types are mapped like the following:

    bytes (dynamic sized byte sequence)                               --> ByteString
    string (dynamic sized unicode string assumed to be UTF-8 encoded) --> String

 TODO: https://github.com/purescript/purescript/issues/2899

--}
