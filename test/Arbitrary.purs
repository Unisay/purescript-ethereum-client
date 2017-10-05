module Arbitrary where

import Prelude
import Data.BigInt as I
import Data.ByteString as B
import Control.Monad.Gen (chooseInt)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.BigInt (BigInt)
import Data.ByteString (Encoding(..), Octet)
import Data.Ethereum (Abi(..), Address, Bytes(..), Code(..), Quantity, TxHash)
import Data.Ethereum.Abi.Type.SignedInt (SignedInt, mkSignedInt)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.String (fromCharArray)
import Data.Typelevel.Num (D8, d64, d8)
import Data.Typelevel.Num.Aliases (D64)
import Ethereum (class ToHex)
import Ethereum.Hex (class FromHex)
import Test.MkUnsafe (class MkUnsafe, mkUnsafe, unsafeJust, unsafeRight)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, elements, suchThat, vectorOf)
import Type.Quotient (mkQuotient)

-- | TODO: generate larger than fits in 32bits
newtype ArbBigInt = ArbBigInt BigInt
instance arbitraryBigInt :: Arbitrary ArbBigInt where
  arbitrary = ArbBigInt <<< I.fromInt <$> arbitrary
derive instance newtypeBigInt :: Newtype ArbBigInt _
derive newtype instance toHexArbBigInt :: ToHex ArbBigInt
derive newtype instance fromHexArbBigInt :: FromHex ArbBigInt

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
  arbitrary = ArbQuantity <$> mkUnsafe <$> suchThat arbitrary (_ >= 0)

newtype HexChar = HexChar Char
derive instance newtypeHexChar :: Newtype HexChar _
instance arbitraryHexChar :: Arbitrary HexChar where
  arbitrary =
    let chrs = ['a', 'b', 'c', 'd', 'e', 'f',
                '1', '2', '3', '4', '5', '6', '7', '8', '9']
    in elements $ HexChar <$> NonEmpty '0' chrs

newtype ArbSignedInt8 = ArbSignedInt8 (SignedInt D8)
derive newtype instance showArbSignedInt8 :: Show ArbSignedInt8
derive instance newtypeSignedInt8 :: Newtype ArbSignedInt8 _
instance mkUnsafeSignedInt8 :: MkUnsafe String ArbSignedInt8 where
  mkUnsafe = I.fromBase 16 >>> unsafeJust >>> I.abs >>> mkSignedInt d8
             >>> unsafeRight >>> ArbSignedInt8
instance  arbitrarySignedInt8 :: Arbitrary ArbSignedInt8 where
  arbitrary = vectorOf 2 (arbitrary :: Gen HexChar) <#>
                map unwrap >>> fromCharArray >>> mkUnsafe

newtype ArbSignedInt64 = ArbSignedInt64 (SignedInt D64)
derive newtype instance showArbSignedInt64 :: Show ArbSignedInt64
derive instance newtypeSignedInt64 :: Newtype ArbSignedInt64 _
instance mkUnsafeSignedInt64 :: MkUnsafe String ArbSignedInt64 where
  mkUnsafe = I.fromBase 16 >>> unsafeJust >>> I.abs >>> mkSignedInt d64
             >>> unsafeRight >>> ArbSignedInt64
instance  arbitrarySignedInt64 :: Arbitrary ArbSignedInt64 where
  arbitrary = vectorOf 8 (arbitrary :: Gen HexChar) <#>
                map unwrap >>> fromCharArray >>> mkUnsafe
