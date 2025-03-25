module Perf.Server where

import Network.Wai
import Network.Wai.Handler.Warp
import RIO qualified
import Network.HTTP.Types
import Data.Function

runWarpDebug :: Port -> Application -> IO ()
runWarpDebug p = runSettings (defaultSettings & setPort p & setOnExceptionResponse exceptionResponseForDebug)

server :: Port -> IO ()
server = flip runWarpDebug app

app :: Application
app request respond =
  respond $
    responseLBS
      status200
      [(hContentType, "text/html")]
      "Hello, World!"
