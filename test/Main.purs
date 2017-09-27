module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Network.HTTP.Affjax (AJAX)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Ethereum.Api.Spec as Api
import Ethereum.Hex.Spec as Hex


main :: ∀ e. Eff ( console    :: CONSOLE
                 , testOutput :: TESTOUTPUT
                 , avar       :: AVAR
                 , random     :: RANDOM
                 , ajax       :: AJAX
                 | e
                 ) Unit
main = runTest do
  Api.spec
  Hex.spec
