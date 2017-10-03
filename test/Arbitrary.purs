module Arbitrary where

import Prelude

import Control.Monad.Gen (chooseInt)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.ByteString (Encoding(..), Octet)
import Data.ByteString as B
import Data.Ethereum (Abi(..), Address, Bytes(..), Code(..), Quantity, TxHash)
import Data.Newtype (class Newtype, unwrap)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, suchThat, vectorOf)
import Test.Unsafe (class MkUnsafe, mkUnsafe)
import Type.Quotient (mkQuotient)


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
