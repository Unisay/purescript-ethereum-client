module Data.Ethereum.Abi.Type.Spec where

import Test.Unit (TestSuite, failure, suite, test)

spec :: ∀ e. TestSuite e
spec = do
  suite "Abi Types" do
    test "fail" do
      failure "Not Omplemented"
