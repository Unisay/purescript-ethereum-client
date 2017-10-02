module Ethereum.Example.SignVerify where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either, either)
import Ethereum as E
import Network.HTTP.Affjax (AJAX)
import Network.Rpc.Json (AffjaxLoggingTransport(..))

type Error = String

main :: ∀ e. Eff (console :: CONSOLE, ajax :: AJAX | e) Unit
main = launchAff_ $ either log exec ethereumProgram
  where exec = E.run $ AffjaxLoggingTransport "http://127.0.0.1:8545"

ethereumProgram :: Either Error (E.Eth Unit) -- TODO: liftEth ?
ethereumProgram = do
  account  <- makeAccountAddress
  endpoint <- makeEndpoint
  let makeMessageToSign = E.mkBytes "I am the Walrus"
  pure $ signVerify account endpoint makeMessageToSign

  where

  makeEndpoint :: Either Error E.Endpoint
  makeEndpoint = E.Endpoint <$> makeContractAddress
                            <*> makeContractAbi

  -- | Account must be unlocked in order to be able to sign messages
  makeAccountAddress :: Either Error E.Address -- My account on Rinkeby
  makeAccountAddress = E.fromHex "b1e9118a35062711e0129e918370a9b1d8e184c7"

  makeContractAddress :: Either Error E.Address -- on Ropsten and Rinkeby
  makeContractAddress = E.fromHex "5481c0fe170641bd2e0ff7f04161871829c1902d"

  {- Contract Code:

    contract SignAndVerifyExample {
        function RecoverAddress( bytes32 msgHash
                               , uint8 v
                               , bytes32 r
                               , bytes32 s
                               ) constant returns (address) {
            return ecrecover(msgHash, v, r, s);
        }
    }
  -}
  makeContractAbi :: Either Error E.Abi
  makeContractAbi = jsonParser abi <#> E.mkAbi where
    abi = """
    [{
        "name": "RecoverAddress",
        "inputs":[
          { "name": "msgHash", "type": "bytes32" },
          { "name": "v",       "type": "uint8"   },
          { "name": "r",       "type": "bytes32" },
          { "name": "s",       "type": "bytes32" }
        ],
        "outputs": [
          { "name": "",        "type": "address" }
        ],
        "constant" : true,
        "payable"  : false,
        "type"     : "function"
    }]
    """

  signVerify :: E.Address -> E.Endpoint -> E.Bytes -> E.Eth Unit
  signVerify account endpoint message = do
    signature <- E.ethSign account message
    let rsv = E.rsv signature
    pure unit
