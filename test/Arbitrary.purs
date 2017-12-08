module Arbitrary where

import Prelude

import Control.Monad.Gen (chooseInt)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.BaseChar (OctChar)
import Data.BigInt (BigInt)
import Data.BigInt as I
import Data.Binary.SignedInt (SignedInt)
import Data.Binary.SignedInt as SI
import Data.Binary.UnsignedInt (UnsignedInt)
import Data.Binary.UnsignedInt as UI
import Data.ByteString (Encoding(..), Octet)
import Data.ByteString as B
import Data.Ethereum (Abi(..), Address, Bytes(..), Code(..), Quantity, TxHash)
import Data.Newtype (class Newtype, unwrap)
import Data.Typelevel.Num (D8, d32, d64)
import Data.Typelevel.Num.Aliases (D64)
import Ethereum.Hex (class ToHex, class FromHex)
import Test.MkUnsafe (class MkUnsafe, mkUnsafe, unsafeJust)
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

newtype ArbUnsignedInt8 = ArbUnsignedInt8 (UnsignedInt D8)
derive newtype instance showArbUnsignedInt8 :: Show ArbUnsignedInt8
derive instance newtypeUnsignedInt8 :: Newtype ArbUnsignedInt8 _
instance mkUnsafeUnsignedInt8 :: MkUnsafe Int ArbUnsignedInt8 where
  mkUnsafe i = ArbUnsignedInt8 $ unsafeJust $ UI.tryAsBits $ UI.fromInt d32 i

instance arbitraryUnsignedInt8 :: Arbitrary ArbUnsignedInt8 where
  arbitrary = mkUnsafe <$> suchThat arbitrary \i -> i >= 0 && i < 255

newtype ArbUnsignedInt64 = ArbUnsignedInt64 (UnsignedInt D64)
derive newtype instance showArbUnsignedInt64 :: Show ArbUnsignedInt64
derive instance newtypeUnsignedInt64 :: Newtype ArbUnsignedInt64 _
instance mkUnsafeUnsignedInt64 :: MkUnsafe Int ArbUnsignedInt64 where
  mkUnsafe i = ArbUnsignedInt64 $ UI.fromInt d64 i

instance arbitraryUnsignedInt64 :: Arbitrary ArbUnsignedInt64 where
  arbitrary = mkUnsafe <$> arbitrary

newtype ArbSignedInt8 = ArbSignedInt8 (SignedInt D8)
derive newtype instance showArbSignedInt8 :: Show ArbSignedInt8
derive instance newtypeSignedInt8 :: Newtype ArbSignedInt8 _
instance mkUnsafeSignedInt8 :: MkUnsafe Int ArbSignedInt8 where
  mkUnsafe i = ArbSignedInt8 $ unsafeJust $ SI.tryAsBits $ SI.fromInt d32 i

instance arbitrarySignedInt8 :: Arbitrary ArbSignedInt8 where
  arbitrary = mkUnsafe <$> suchThat arbitrary \i -> i >= (-128) && i < 128

newtype ArbSignedInt64 = ArbSignedInt64 (SignedInt D64)
derive newtype instance showArbSignedInt64 :: Show ArbSignedInt64
derive instance newtypeSignedInt64 :: Newtype ArbSignedInt64 _
instance mkUnsafeSignedInt64 :: MkUnsafe Int ArbSignedInt64 where
  mkUnsafe i = ArbSignedInt64 $ SI.fromInt d64 i

instance arbitrarySignedInt64 :: Arbitrary ArbSignedInt64 where
  arbitrary = mkUnsafe <$> arbitrary
