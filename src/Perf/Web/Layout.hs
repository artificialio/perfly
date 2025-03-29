module Perf.Web.Layout where

import Yesod.Lucid
import Perf.Web.Types

defaultLayout_ :: HtmlT (Reader (Page App)) a -> HtmlT (Reader (Page App)) a
defaultLayout_ body = do
  style_ "body {font-family: sans-serif}"
  body
