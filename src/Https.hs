module Https where

import Data.ByteString.Lazy
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple

httpsGet :: Manager -> String -> IO ByteString
httpsGet manager url = do
  re <- parseRequest url
  let request = setRequestManager manager re
  response <- httpLBS request
  return $ getResponseBody response
