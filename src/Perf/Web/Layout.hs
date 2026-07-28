module Perf.Web.Layout where

import Data.List qualified as List
import Data.Text (Text)
import Lucid.Base
import NeatInterpolation (trimming)
import Perf.Types.Web
import Yesod.Lucid

defaultLayout_ :: Text -> HtmlT (Reader (Page App)) a -> HtmlT (Reader (Page App)) a
defaultLayout_ title body = do
  doctypehtml_ do
    headCommon_ title
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

staticLayout_ :: Text -> Html () -> Html ()
staticLayout_ title body =
  doctypehtml_ do
    headCommon_ title
    body_ do
      h1_ $ toHtml title
      body

headCommon_ :: Monad m => Text -> HtmlT m ()
headCommon_ title =
  head_ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ $ toHtml title
    style_ commonStyles
    script_
      [ src_ "https://cdn.jsdelivr.net/npm/plotly.js-dist-min@2.35.2/plotly.min.js",
        crossorigin_ "anonymous",
        makeAttributes "referrerpolicy" "no-referrer"
      ]
      (mempty :: Text)

commonStyles :: Text
commonStyles =
  [trimming|
    body {font-family: monospace; margin: 24px; max-width: none; background: #f6f7f9; color: #111827;}
    table.metrics td, table.metrics th {border: 1px solid black; padding: 2px;}
    .benchmark-subject {background: #ffffff; border: 1px solid #e5e7eb; border-radius: 10px; padding: 14px 14px 18px 14px; margin-bottom: 1.25rem;}
    .benchmark-subject > h2 {margin: 0 0 8px 0;}
    .chart-grid {display: grid; grid-template-columns: repeat(auto-fit, minmax(420px, 1fr)); gap: 1.75rem 12px; align-items: start;}
    .chart-cell {min-width: 0; margin-bottom: 0.5rem;}
    .benchmark-plot {width: 100%; min-height: 340px;}
    .factor-lines {margin-top: 2px; margin-bottom: 0.75rem; color: #6b7280; font-size: 0.92rem; line-height: 1.35;}
    .factor-line {margin-top: 4px;}
    .factor-toggle {border: none; background: transparent; color: #4b5563; cursor: pointer; font: inherit; display: inline-flex; align-items: center; gap: 8px; padding: 0; text-align: left;}
    .factor-toggle.off {opacity: 0.5; text-decoration: line-through;}
    .factor-swatch {width: 14px; height: 2px; border-radius: 2px; flex: 0 0 auto;}
  |]
