module Data.Ethereum.Abi.Type.SignedInt.Spec where

import Prelude
import Data.String as S
import Arbitrary (ArbSignedInt64, ArbSignedInt8)
import Control.Monad.Eff.Random (RANDOM)
import Data.BigInt (BigInt, fromInt)
import Data.Either (either, isLeft, isRight)
import Data.Ethereum.Abi.Class (class AbiType, enc)
import Data.Ethereum.Abi.Type.Class (class Dividend8)
import Data.Ethereum.Abi.Type.SignedInt (SignedInt, invert, isNegative, mkSignedInt)
import Data.Newtype (unwrap)
import Data.Typelevel.Num (d16, d8)
import Ethereum.Hex (class FromHex, fromHex)
import Test.QuickCheck (class Arbitrary, Result(..), arbitrary, (<?>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "SignedInt" do
    test "mkSignedInt 8" $
      quickCheck propSignedInt8
    test "mkSignedInt 16" $
      quickCheck propSignedInt16
    test "invert SignedInt 8" $
      quickCheck ((unwrap >>> propInvert) :: ArbSignedInt8 -> Result)
    test "invert SignedInt 64" $
      quickCheck ((unwrap >>> propInvert) :: ArbSignedInt64 -> Result)
    test "enc SignedInt 8 is multiple 32" $
      quickCheck ((unwrap >>> propTypeEncMultiple32b) :: ArbSignedInt8 -> Result)
    test "enc SignedInt 64 is multiple 32" $
      quickCheck ((unwrap >>> propTypeEncMultiple32b) :: ArbSignedInt64 -> Result)
    test "enc SignedInt 8 is decodable" $
      quickCheck ((unwrap >>> propDecodableEnc) :: ArbSignedInt8 -> Result)
    test "enc SignedInt 64 is decodable" $
      quickCheck ((unwrap >>> propDecodableEnc) :: ArbSignedInt64 -> Result)

newtype ArbBigInt = ArbBigInt BigInt

instance arbitraryBigInt :: Arbitrary ArbBigInt where
  arbitrary = ArbBigInt <<< fromInt <$> arbitrary

propSignedInt8 :: ArbBigInt -> Boolean
propSignedInt8 (ArbBigInt i) =
  let v = mkSignedInt d8 i
  in if (fromInt (-128) <= i && i < fromInt 128)
     then isRight v
     else isLeft v

propSignedInt16 :: ArbBigInt -> Boolean
propSignedInt16 (ArbBigInt i) =
  let v = mkSignedInt d16 i
  in if (fromInt (-32768) <= i && i < fromInt 32768)
     then isRight v
     else isLeft v

propTypeEncMultiple32b :: ∀ a. AbiType a => Show a => a -> Result
propTypeEncMultiple32b t =
  let digits = S.length (enc t) - 2
      res = digits `mod` 2 == 0 && digits `mod` 64 == 0
  in res <?> ("propTypeEncMultiple32b did not hold for " <> show t)

propDecodableEnc :: ∀ a. AbiType a => Eq a => Show a => FromHex a => a -> Result
propDecodableEnc signed = either (Failed <<< append (enc signed <> ": "))
                                   (const Success) do
  decoded <- fromHex $ enc signed
  pure $ decoded == signed

propInvert :: ∀ a. Dividend8 a => SignedInt a -> Result
propInvert signed =
  let inverted = invert signed
  in (isNegative signed == isNegative inverted) <?> "inverted SignedInt changed sign"
