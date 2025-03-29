{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module Perf.Web where

import System.Environment
import qualified Data.Text as T
import Database.Persist.Sqlite
import Perf.Web.Dispatch ()
import Perf.Web.Types
import RIO qualified
import Yesod (warpEnv)
import qualified Perf.Types.DB as DB

server :: IO ()
server = do
  token <- getEnv "PERF_TOKEN"
  let dbPath = "perf.sqlite3"
  runSqlite dbPath $ runMigration DB.migrateAll
  dbLock <- RIO.newMVar dbPath
  warpEnv App { dbLock, token = T.pack token }
