module Perf.Web.Layout where

import Lucid.Base
import qualified Data.List as List
import Data.Text (Text)
import Yesod.Lucid
import Perf.Types.Web

defaultLayout_ :: Text -> HtmlT (Reader (Page App)) a -> HtmlT (Reader (Page App)) a
defaultLayout_ title body = do
  doctypehtml_ do
    head_ do
      title_ $ toHtml title
      style_ "body {font-family: monospace; margin: 0 auto; max-width: 800px;}"
      script_ [
        src_ "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.min.js",
        integrity_ "CQBWl4fJHWbryGE+Pc7UAxWMUMNMWzWxF4SQo9CgkJIN1kx6djDQZjh3Y8SZ1d+6I+1zze6Z7kHXO7q3UyZAWw==",
        crossorigin_ "anonymous",
        makeAttributes "referrerpolicy" "no-referrer"] (mempty :: Text)
    body_ do
      h1_ $ toHtml title
      crumbs <- asks (.crumbs)
      unless (null crumbs) $
        p_ do
          url <- asks (.url)
          let loaf = flip map crumbs \(route,display) ->
                a_ [href_ (url route)] $ toHtml display
          sequence_ $ List.intersperse (em_ " / ") loaf
      body
