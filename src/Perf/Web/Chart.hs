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
    -- | One entry per line: its series key, legend name and colour.
    traces :: Value,
    -- | Plotly layout without the x-axis categories, which depend on how many
    -- master commits are shown.
    layout :: Value,
    heightPx :: Int
  }

-- | Shared Plotly config (also embedded in the plot-controls script).
plotlyConfig :: Value
plotlyConfig =
  object
    [ "responsive" .= True,
      "modeBarButtonsToRemove" .= (["select2d", "lasso2d"] :: [Text])
    ]

plotlyConfigJson :: Text
plotlyConfigJson = encode' plotlyConfig

-- | Emit an empty plot container. The shared controls script in 'Perf.Web.Plot'
-- fills it with @Plotly.newPlot@ / @Plotly.react@ once it scrolls into view,
-- reading the points from the page-level plot data.
chart_ :: ChartOptions -> Html ()
chart_ opts = do
  div_
    [ id_ opts.chartId,
      class_ "benchmark-plot",
      style_ $ "height: " <> T.pack (show opts.heightPx) <> "px;",
      makeAttributes "data-traces" (encode' opts.traces),
      makeAttributes "data-layout" (encode' opts.layout)
    ]
    (pure ())

encode' :: ToJSON a => a -> Text
encode' = T.decodeUtf8 . L.toStrict . encode
