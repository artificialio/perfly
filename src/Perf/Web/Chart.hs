module Perf.Web.Chart where

import Data.Aeson
import Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Lucid

chart_ :: Text -> Value -> Value -> Html ()
chart_ idx plotData layout = do
  div_ [id_ idx, style_ "height: 300px; max-width: 400px;"] do
    pure ()
  script_ $
    T.concat
      [ "(() => {",
        "const data = ",
        encode' plotData,
        ";",
        "const layout = ",
        encode' layout,
        ";",
        "const config = {responsive: true, modeBarButtonsToRemove: ['select2d', 'lasso2d']};",
        "Plotly.newPlot(",
        encode' idx,
        ", data, layout, config);",
        "})();"
      ]

encode' :: ToJSON a => a -> Text
encode' = T.decodeUtf8 . L.toStrict . encode
