module Ethereum
  ( module E
  , module D
  , module H
  -- , deployContract
  ) where

-- import Prelude

import Ethereum.Api as E
import Ethereum.Hex as H
import Data.Ethereum as D
--
-- deployContract :: E.Address -> E.Abi -> E.Eth E.Contract
-- deployContract addr abi = do
--   let tx = E.Transaction { from: addr
--                          , to:
--
--                          }
--   hash <- E.ethSendTransaction tx
--   -- TODO: check if code has been deployed
--   pure $ E.Contract addr abi hash