{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language DataKinds #-}

module Perf.Types.DB where

import Data.Traversable
import Control.Monad.IO.Class
import Database.Persist.TH
import Database.Persist.Sqlite
import Database.Persist
import Data.Time (UTCTime)
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Branch
  name Text
  createdAt UTCTime
  UniqueBranch name
  deriving Show

Commit
  hash Text
  createdAt UTCTime
  UniqueCommit hash
  deriving Show

MapBranchCommit
  branchId BranchId
  commitId CommitId
  UniqueMapBranchCommit branchId commitId
  deriving Show
  deriving Eq

Benchmark
  commitId CommitId
  subject Text
  deriving Show

Test
  benchmarkId BenchmarkId
  deriving Show

Factor
  testId TestId
  name Text
  value Text
  deriving Show

Metric
  testId TestId
  name Text
  rangeLower Double
  rangeUpper Double
  mean Double
  stddev Double
  deriving Show
|]
