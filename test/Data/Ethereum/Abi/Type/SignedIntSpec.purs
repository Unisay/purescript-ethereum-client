module Data.Ethereum.Abi.Type.SignedInt.Spec where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.BigInt (BigInt, fromInt)
import Data.Either (either, isLeft, isRight)
import Data.Ethereum.Abi.Class (class AbiType, enc)
import Data.Ethereum.Abi.Type (SignedInt, mkSignedInt)
import Data.String as S
import Data.Typelevel.Num (D8, D16, d16, d8)
import Ethereum.Hex (class FromHex, fromHex)
import Test.QuickCheck (class Arbitrary, Result(..), arbitrary, (<?>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "SignedInt" do
    test "mkSignedInt D8"   $ quickCheck propSignedInt8
    test "mkSignedInt D16"  $ quickCheck propSignedInt16
    test "enc SignedInt 8"  $ quickCheck propSignedIntEnc8
    test "enc SignedInt 16" $ quickCheck propSignedIntEnc16
    test "enc SignedInt 8 is decodable"  $ quickCheck propDecodableEnc8
    test "enc SignedInt 16 is decodable" $ quickCheck propDecodableEnc16

newtype ArbBigInt = ArbBigInt BigInt

instance arbitraryBigInt :: Arbitrary ArbBigInt where
  arbitrary = ArbBigInt <<< fromInt <$> arbitrary

propSignedInt8 :: ArbBigInt -> Boolean
propSignedInt8 (ArbBigInt i) =
  let v = mkSignedInt d8 i in if (zero <= i && i < fromInt 256) then isRight v else isLeft v

propSignedInt16 :: ArbBigInt -> Boolean
propSignedInt16 (ArbBigInt i) =
  let v = mkSignedInt d16 i in if (zero <= i && i < fromInt 65536) then isRight v else isLeft v

propSignedIntEnc8 :: SignedInt D8 -> Result
propSignedIntEnc8 = propTypeEncMultiple32b

propSignedIntEnc16 :: SignedInt D16 -> Result
propSignedIntEnc16 = propTypeEncMultiple32b

propDecodableEnc8 :: SignedInt D8 -> Result
propDecodableEnc8 = propDecodableEnc

propDecodableEnc16 :: SignedInt D16 -> Result
propDecodableEnc16 = propDecodableEnc

propTypeEncMultiple32b :: ∀ a. AbiType a => Show a => a -> Result
propTypeEncMultiple32b t =
  let digits = S.length (enc t) - 2
      res = digits `mod` 2 == 0 && digits `mod` 64 == 0
  in res <?> ("propTypeEncMultiple32b did not hold for " <> show t)

propDecodableEnc :: ∀ a. AbiType a => Eq a => Show a => FromHex a => a -> Result
propDecodableEnc unsigned = either Failed (const Success) do
  decoded <- fromHex $ enc unsigned
  pure $ decoded == unsigned

-- TODO: write properties to check padding for positive and negative cases
