module Arbitrary where

import Prelude
import Data.BigInt as I
import Data.ByteString as B
import Control.Monad.Gen (chooseInt)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.BaseChar (OctChar)
import Data.BigInt (BigInt)
import Data.ByteString (Encoding(..), Octet)
import Data.Ethereum (Abi(..), Address, Bytes(..), Code(..), Quantity, TxHash)
import Data.Ethereum.Abi.Type.SignedInt (SignedInt, mkSignedInt)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D8, d64, d8)
import Data.Typelevel.Num.Aliases (D64)
import Ethereum.Hex (class ToHex, class FromHex)
import Test.MkUnsafe (class MkUnsafe, mkUnsafe, unsafeRight)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, suchThat, vectorOf)
import Type.Quotient (mkQuotient)

newtype ArbBigInt = ArbBigInt BigInt
derive instance newtypeBigInt :: Newtype ArbBigInt _
derive newtype instance toHexArbBigInt :: ToHex ArbBigInt
derive newtype instance fromHexArbBigInt :: FromHex ArbBigInt
instance arbitraryBigInt :: Arbitrary ArbBigInt where
  arbitrary = do
    n <- chooseInt 1 256
    cs <- vectorOf n (arbitrary :: Gen OctChar)
    b <- arbitrary
    let bi = mkUnsafe cs
    pure $ ArbBigInt $ if b then negate bi else bi


newtype NonNegativeBigInt = NonNegativeBigInt BigInt
instance arbitraryNonNegativeBigInt :: Arbitrary NonNegativeBigInt where
arbitrary = do
  n <- chooseInt 1 256
  cs <- vectorOf n (arbitrary :: Gen OctChar)
  pure $ NonNegativeBigInt $ mkUnsafe cs


newtype Byte = Byte Octet
derive instance newtypeByte :: Newtype Byte _
instance arbitraryByte :: Arbitrary Byte where
  arbitrary = Byte <<< mkQuotient <$> chooseInt 0 255

newtype ArbBytes = ArbBytes Bytes
derive instance newtypeArbBytes :: Newtype ArbBytes _
instance arbitraryArbBytes :: Arbitrary ArbBytes where
  arbitrary = ArbBytes <$> Bytes <$> arbitrary

newtype ArbAddress = ArbAddress Address
derive instance newtypeArbAddress :: Newtype ArbAddress _
derive newtype instance showArbAddress :: Show ArbAddress
derive newtype instance eqArbAddress :: Eq ArbAddress
derive newtype instance toHexArbAddress :: ToHex ArbAddress
derive newtype instance fromHexArbAddress :: FromHex ArbAddress
derive newtype instance encodeJsonArbAddress :: EncodeJson ArbAddress
derive newtype instance decodeJsonArbAddress :: DecodeJson ArbAddress
instance arbitraryArbAddress :: Arbitrary ArbAddress where
  arbitrary = ArbAddress <$> arbitratyBytes 20

newtype ArbAbi = ArbAbi Abi
derive instance newtypeArbAbi :: Newtype ArbAbi _
derive newtype instance showArbAbi :: Show ArbAbi
derive newtype instance eqArbAbi :: Eq ArbAbi
derive newtype instance encodeJsonArbAbi :: EncodeJson ArbAbi
instance arbitraryArbAbi :: Arbitrary ArbAbi where
  arbitrary = ArbAbi <$> Abi <$> map (unwrap :: ArbBytes -> Bytes) arbitrary

newtype ArbCode = ArbCode Code
derive instance newtypeArbCode :: Newtype ArbCode _
derive newtype instance showArbCode :: Show ArbCode
derive newtype instance eqArbCode :: Eq ArbCode
derive newtype instance encodeJsonArbCode :: EncodeJson ArbCode
instance arbitraryArbCode :: Arbitrary ArbCode where
  arbitrary = ArbCode <$> Code <$> map (unwrap :: ArbBytes -> Bytes) arbitrary

newtype ArbTxHash = ArbTxHash TxHash
derive instance newtypeArbTxHash :: Newtype ArbTxHash _
derive newtype instance showArbTxHash :: Show ArbTxHash
derive newtype instance eqArbTxHash :: Eq ArbTxHash
derive newtype instance encodeJsonArbTxHash :: EncodeJson ArbTxHash
derive newtype instance decodeJsonArbTxHash :: DecodeJson ArbTxHash
instance arbitraryArbTxHash :: Arbitrary ArbTxHash where
  arbitrary = ArbTxHash <$> arbitratyBytes 32

arbitratyBytes :: ∀ a. MkUnsafe String a => Int -> Gen a
arbitratyBytes n = vectorOf n (arbitrary :: Gen Byte) <#>
  mkUnsafe <<< (\s -> B.toString s Hex) <<< B.pack <<< map unwrap

newtype ArbQuantity = ArbQuantity Quantity
derive instance newtypeQuantity :: Newtype ArbQuantity _
derive newtype instance eqArbQuantity :: Eq ArbQuantity
derive newtype instance showArbQuantity :: Show ArbQuantity
derive newtype instance encodeJsonArbQuantity :: EncodeJson ArbQuantity
derive newtype instance decodeJsonArbQuantity :: DecodeJson ArbQuantity
instance arbitraryArbQuantity :: Arbitrary ArbQuantity where
  arbitrary = ArbQuantity <$> mkUnsafe <<< I.fromInt <$> suchThat arbitrary (_ >= 0)

newtype ArbSignedInt8 = ArbSignedInt8 (SignedInt D8)
derive newtype instance showArbSignedInt8 :: Show ArbSignedInt8
derive instance newtypeSignedInt8 :: Newtype ArbSignedInt8 _
derive newtype instance ringArbSignedInt8 :: Ring ArbSignedInt8
instance mkUnsafeSignedInt8 :: MkUnsafe (Tuple Boolean (Array OctChar)) ArbSignedInt8 where
  mkUnsafe (Tuple isNegative chars) =
    let mkIt = mkUnsafe >>> I.abs >>> mkSignedInt d8 >>> unsafeRight >>> ArbSignedInt8
        si = mkIt chars
    in if isNegative then negate si else si

instance arbitrarySignedInt8 :: Arbitrary ArbSignedInt8 where
  arbitrary = do
    chrs <- vectorOf 1 (arbitrary :: Gen OctChar)
    sign <- arbitrary
    pure $ mkUnsafe $ Tuple sign chrs

newtype ArbSignedInt64 = ArbSignedInt64 (SignedInt D64)
derive newtype instance showArbSignedInt64 :: Show ArbSignedInt64
derive instance newtypeSignedInt64 :: Newtype ArbSignedInt64 _
derive newtype instance ringArbSignedInt64 :: Ring ArbSignedInt64
instance mkUnsafeSignedInt64 :: MkUnsafe (Tuple Boolean (Array OctChar)) ArbSignedInt64 where
  mkUnsafe (Tuple isNegative chars) =
    let mkIt = mkUnsafe >>> I.abs >>> mkSignedInt d64 >>> unsafeRight >>> ArbSignedInt64
        si = mkIt chars
    in if isNegative then negate si else si
instance arbitrarySignedInt64 :: Arbitrary ArbSignedInt64 where
  arbitrary = do
    chrs <- vectorOf 8 (arbitrary :: Gen OctChar)
    sign <- arbitrary
    pure $ mkUnsafe $ Tuple sign chrs
