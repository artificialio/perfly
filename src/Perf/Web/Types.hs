module Perf.Web.Types where

import Data.Text (Text)
import RIO qualified

data App = App {
  dbLock :: RIO.MVar Text,
  token :: Text
  }
