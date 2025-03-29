module Perf.Web.Db where

import qualified UnliftIO
import qualified Perf.Types.DB as DB
import Database.Persist.Sqlite
import Yesod
import Perf.Web.Types
import Perf.Web.Foundation

db :: DB.DB a -> Handler a
db m = do
  app <- getYesod
  liftIO $
    UnliftIO.withMVar app.dbLock
      \fp -> runSqlite fp m
