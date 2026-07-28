module Perf.Web.Chart where

import Data.Aeson
import Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Lucid
import Lucid.Base (makeAttributes)

data ChartOptions = ChartOptions
  { chartId :: Text,
    plotData :: Value,
    layout :: Value,
    masterCount :: Int,
    heightPx :: Int
  }

-- | Emit a plot container. Plotting is performed by the shared controls script
-- in 'Perf.Web.Plot', which reads the data-* attributes.
chart_ :: ChartOptions -> Html ()
chart_ opts = do
  div_
    [ id_ opts.chartId,
      class_ "benchmark-plot",
      style_ $ "height: " <> T.pack (show opts.heightPx) <> "px;",
      makeAttributes "data-full" (encode' opts.plotData),
      makeAttributes "data-layout" (encode' opts.layout),
      makeAttributes "data-master-count" (T.pack (show opts.masterCount))
    ]
    (pure ())

encode' :: ToJSON a => a -> Text
encode' = T.decodeUtf8 . L.toStrict . encode
