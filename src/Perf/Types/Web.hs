module Perf.Types.Web where

import Data.Text (Text)
import RIO qualified

data App = App {
  dbLock :: RIO.MVar Text,
  token :: Text
  }
