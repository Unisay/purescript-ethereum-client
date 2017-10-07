module Ethereum.Hex.Spec where

import Prelude

import Arbitrary (ArbAddress, NonNegativeBigInt(NonNegativeBigInt))
import Control.Monad.Eff.Random (RANDOM)
import Data.BigInt as I
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Either (Either(Right))
import Data.Ethereum (Address)
import Data.String.Utils (unsafeRepeat)
import Data.Traversable (traverse)
import Ethereum.Hex (fromHex, toHex)
import Property (isHexBytesEncoding, isHex)
import Test.MkUnsafe (mkUnsafe)
import Test.QuickCheck (Result(Success, Failed))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "To Hex" do
    let byteString :: ByteString
        byteString = mkUnsafe "02cafebabe"
        bigInt = I.fromInt 934069
    test "toHex ByteString" $
      "0x02cafebabe" `equal` toHex byteString
    test "toHex ByteString produces a correct hex encoding" $
      quickCheck ((toHex >>> isHexBytesEncoding) :: ByteString -> Result)
    test "toHex BigInt" $
      "0xe40b5" `equal` toHex bigInt
    test "toHex BigInt produces a correct hexadecimal quantity encoding" $
      quickCheck propNonNegativeBigIntHexEncoding
    test "toHex Address" do
      let addressStr = unsafeRepeat 20 "20"
          address = mkUnsafe addressStr :: Address
      ("0x" <> addressStr) `equal` toHex address
    test "toHex Address produces a correct hex encoding" $
      quickCheck ((toHex >>> isHexBytesEncoding) :: ArbAddress -> Result)

  suite "From Hex" do
    test "fromHex ByteString" do
      let expected = Right [mkUnsafe "02cafebabe" :: BS.ByteString, mkUnsafe "00", mkUnsafe "0e40b5"]
      expected `equal` traverse fromHex ["0x02cafebabe", "0x0", "e40b5"]
    test "fromHex BigInt" do
      let expected = Right [I.fromInt 1024, I.fromInt 1024, I.fromInt 0, I.fromInt 934069]
      expected `equal` traverse fromHex ["0x400", "0x0400", "0x0", "0xe40b5"]
    test "fromHex Int" do
      let expected = Right [1024, 1024, 0, 934069]
      expected `equal` traverse fromHex ["0x400", "0x0400", "0x0", "0xe40b5"]


propNonNegativeBigIntHexEncoding :: NonNegativeBigInt -> Result
propNonNegativeBigIntHexEncoding (NonNegativeBigInt bi) =
  clarify ("propNonNegativeBigIntHexEncoding (" <> (I.toString bi) <> "): ") $ isHex $ toHex bi

clarify :: String -> Result -> Result
clarify s (Failed e) = Failed $ s <> e
clarify _ Success = Success
