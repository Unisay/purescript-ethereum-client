module Data.Ethereum.Abi.Type.Class
  ( class Dividend8
  ) where

import Data.Typelevel.Num (class LtEq, class Mod, class Pos, type (:*), D0, D2, D5, D6, D8)

class (Pos m, LtEq m (D2 :* D5 :* D6), Mod m D8 D0) <= Dividend8 m

instance dividend8TypeLevel :: (Pos m, LtEq m (D2 :* D5 :* D6), Mod m D8 D0) => Dividend8 m
