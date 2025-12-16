module Perf.Web.Layout where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Lucid.Base
import Perf.Types.Web
import Yesod.Lucid

defaultLayout_ :: Text -> HtmlT (Reader (Page App)) a -> HtmlT (Reader (Page App)) a
defaultLayout_ title body = do
  doctypehtml_ do
    head_ do
      meta_ [charset_ "utf-8"]
      title_ $ toHtml title
      style_ $
        T.unwords
          [ "body {font-family: monospace; margin: 0 auto; max-width: 800px;}",
            "table.metrics td, table.metrics th {border: 1px solid black; padding: 2px;}"
          ]
      script_
        [ src_ "https://cdn.jsdelivr.net/npm/chart.js@4.5.1/dist/chart.umd.min.js",
          integrity_ "sha256-SERKgtTty1vsDxll+qzd4Y2cF9swY9BCq62i9wXJ9Uo=",
          crossorigin_ "anonymous",
          makeAttributes "referrerpolicy" "no-referrer"
        ]
        (mempty :: Text)
      script_
        [ src_ "https://cdn.jsdelivr.net/npm/chartjs-plugin-crosshair@2.0.0/dist/chartjs-plugin-crosshair.min.js",
          integrity_ "sha256-5bTtdEYtbjO36pQbMCXOsoYW5u5jfYfyI41LelMTTbQ=",
          crossorigin_ "anonymous",
          makeAttributes "referrerpolicy" "no-referrer"
        ]
        (mempty :: Text)
      script_ [type_ "text/javascript"] $ do
        toHtmlRaw ("Chart.defaults.font.family = 'monospace';" :: Text)
    body_ do
      h1_ $ toHtml title
      crumbs <- asks (.crumbs)
      unless (null crumbs) $
        p_ do
          url <- asks (.url)
          let loaf = flip map crumbs \(route, display) ->
                a_ [href_ (url route)] $ toHtml display
          sequence_ $ List.intersperse (em_ " / ") loaf
      body
