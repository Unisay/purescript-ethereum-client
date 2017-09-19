module Ethereum.Api.Web3 (
    EthF
  , runEth
  , clientVersion
  ) where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (either)
import Data.List (List(..))
import Network.HTTP.Affjax (AJAX, URL, post)
import Network.HTTP.StatusCode (StatusCode(..))

type ClientVersion = String

data EthF more =
  Web3ClientVersion (ClientVersion -> more)

type Eth a = Free EthF a

newtype RpcRequest = RpcRequest { method :: String
                                , params :: List String
                                }

instance encodeRpcRequest :: EncodeJson RpcRequest where
  encodeJson (RpcRequest req) =
       "id" := 1
    ~> "method" := req.method
    ~> "params" := (encodeJson req.params)
    ~> "jsonrpc" := "2.0"
    ~> jsonEmptyObject

instance showRpcRequest :: Show RpcRequest where
  show = show <<< encodeJson

newtype RpcResponse = RpcResponse { result :: String }

instance decodeRpcResponse :: DecodeJson RpcResponse where
  decodeJson json = do
    obj <- decodeJson json
    res <- obj .? "result"
    pure $ RpcResponse { result : res }

clientVersion :: Eth String
clientVersion = liftF $ Web3ClientVersion id

rpcCall :: ∀ e. URL -> RpcRequest -> Aff (ajax :: AJAX | e) RpcResponse
rpcCall url req = do
  {status: (StatusCode statusCode), response: body} <- post url (encodeJson req)
  when (statusCode /= 200) do
    throwError $ error $ "JSON RPC call " <> (show req) <> " failed with HTTP status code = " <> show statusCode
  either (throwError <<< error) pure $ decodeJson body

method :: String -> RpcRequest
method m = RpcRequest { method: m, params: Nil }

fromEthF :: ∀ e. URL -> EthF ~> Aff (ajax :: AJAX | e)
fromEthF url (Web3ClientVersion f) = rpcCall url (method "web3_clientVersion") <#> (\(RpcResponse r) -> f r.result)

runEth :: ∀ e a. URL -> Eth a -> Aff (ajax :: AJAX | e) a
runEth url = foldFree (fromEthF url)
