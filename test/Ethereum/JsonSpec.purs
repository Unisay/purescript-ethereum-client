module Ethereum.Json.Spec where

import Prelude

import Arbitrary (ArbAddress, ArbQuantity, ArbTxHash)
import Control.Monad.Eff.Random (RANDOM)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..), either)
import Data.Ethereum (Transaction(Transaction))
import Data.Maybe (Maybe(..))
import Data.String.Utils (unsafeRepeat)
import Test.QuickCheck (Result(..), assertEquals)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)
import Test.Unsafe (mkUnsafe)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "Json" do
  let address1 = mkUnsafe $ unsafeRepeat 20 "11"
      address2 = mkUnsafe $ unsafeRepeat 20 "22"
      transaction = Transaction { from: address1
                                , to: Just address2
                                , gas: Just $ mkUnsafe 2
                                , gasPrice: Just $ mkUnsafe 3
                                , value: Just $ mkUnsafe 4
                                , data: Left $ mkUnsafe "05"
                                , nonce: Just $ mkUnsafe 6
                              }
  test "encode transaction" do
    let expected = mkUnsafe """
        {
          "from": "0x1111111111111111111111111111111111111111",
          "to": "0x2222222222222222222222222222222222222222",
          "gas": "0x2",
          "gasPrice": "0x3",
          "value": "0x4",
          "data": "0x05",
          "nonce": "0x6"
        }
        """
    expected `equal` encodeJson transaction

  test "roundtrip Address" $
    quickCheck (roundTrip :: ArbAddress -> Result)

  test "roundtrip TxHash" $
    quickCheck (roundTrip :: ArbTxHash -> Result)

  test "roundtrip Quantity" $
    quickCheck (roundTrip :: ArbQuantity -> Result)

  -- TODO: add tests

roundTrip :: ∀ a. Show a => Eq a => EncodeJson a => DecodeJson a => a -> Result
roundTrip a =
  let json = encodeJson a
      fail e = Failed $ "Failed to decode " <> (show json) <> ": " <> e
      verify = assertEquals a
  in either fail verify $ decodeJson json
