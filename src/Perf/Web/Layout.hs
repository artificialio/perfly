module Perf.Web.Layout where

import qualified Data.List as List
import Data.Text (Text)
import Yesod.Lucid
import Perf.Types.Web

defaultLayout_ :: Text -> HtmlT (Reader (Page App)) a -> HtmlT (Reader (Page App)) a
defaultLayout_ title body = do
  doctypehtml_ do
    head_ do
      title_ $ toHtml title
      style_ "body {font-family: sans-serif}"
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
