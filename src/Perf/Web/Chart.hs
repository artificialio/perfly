module Perf.Web.Chart where

import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Lucid

chart_ :: Text -> Value -> Html ()
chart_ idx config = do
  div_ [class_ "width: 400px; height: 400px; margin: 0 auto; padding: 20px;"] do
    canvas_ [id_ idx, style_ "height: 250px; max-width: 400px;"] do
      pure ()
  script_ $
    T.concat [
      "(() => {",
      "const config = ", encode' config, ";",
      "const ctx = document.getElementById(", encode' idx, ").getContext('2d');",
      "new Chart(ctx, config);"
      , "})();"
    ]

encode' :: ToJSON a => a -> Text
encode' = T.decodeUtf8 . L.toStrict . encode
