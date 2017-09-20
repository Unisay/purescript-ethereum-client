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

newtype Request = Request { method :: String
                          , params :: Array String
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

data Response = Result String
              | Error Int String

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj <- decodeJson json
    res <- obj .?? "result"
    maybe (decodeError obj) (Right <<< Result) res
      where decodeError obj = do
              err <- obj .? "error"
              code <- err .? "code"
              message <- err .? "message"
              pure $ Error code message

call :: ∀ e. URL -> Request -> Aff (ajax :: AJAX | e) Response
call url req = do
  {status: (StatusCode statusCode), response: body} <- post url (encodeJson req)
  when (statusCode /= 200) do
    throwError $ error $ "JSON RPC call "
                       <> (show req)
                       <> " failed with HTTP status code = "
                       <> show statusCode
  either (throwError <<< error) pure $ decodeJson body

method :: String -> Request
method m = Request { method: m, params: [] }
