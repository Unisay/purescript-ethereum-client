module Ethereum.Rpc where

import Prelude
import Control.Monad.Aff (Aff, error, throwError)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?), (.??))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Network.HTTP.Affjax (AJAX, URL, post)
import Network.HTTP.StatusCode (StatusCode(..))

type Method = String
type Params = Array String

newtype Request = Request { method :: Method
                          , params :: Params
                          }

instance encodeRequest :: EncodeJson Request where
  encodeJson (Request req) =
       "id" := 1
    ~> "method" := req.method
    ~> "params" := (encodeJson req.params)
    ~> "jsonrpc" := "2.0"
    ~> jsonEmptyObject

instance showRequest :: Show Request where
  show = show <<< encodeJson

data Response a = Result a
                | Error Int String -- TODO: Error codes

instance decodeResponse :: DecodeJson r => DecodeJson (Response r) where
  decodeJson json = do
    obj <- decodeJson json
    res <- obj .?? "result"
    maybe (decodeError obj) (Right <<< Result) res
      where decodeError obj = do
              err <- obj .? "error"
              code <- err .? "code"
              message <- err .? "message"
              pure $ Error code message

call :: ∀ r e. DecodeJson r => URL -> Method -> Params -> Aff (ajax :: AJAX | e) (Response r)
call url method params = do
  let req = encodeJson $ Request { method, params }
  { status: (StatusCode statusCode), response: body } <- post url req
  when (statusCode /= 200) do
    throwError $ error $ "JSON RPC call "
                       <> (show req)
                       <> " failed with HTTP status code = "
                       <> show statusCode
  either (throwError <<< error) pure $ decodeJson body

call0 :: ∀ r e. DecodeJson r => URL -> Method -> Aff (ajax :: AJAX | e) (Response r)
call0 u m = call u m []
