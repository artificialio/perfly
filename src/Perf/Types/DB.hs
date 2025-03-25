{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language DataKinds #-}

module Perf.Types.DB where

import Perf.Types.Prim
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Time (UTCTime)
import Data.Text (Text)
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader
import Control.Monad.Logger

-- I consider this a bit of an anti-pattern, but I don't care enough
-- to use something like persistent-sql-lifted. It's fine.
type DB = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Branch
  name Text
  createdAt UTCTime
  UniqueBranch name
  deriving Show

Commit
  hash Hash
  createdAt UTCTime
  UniqueCommit hash
  deriving Show
  deriving Eq

MapBranchCommit
  branchId BranchId
  commitId CommitId
  UniqueMapBranchCommit branchId commitId
  deriving Show
  deriving Eq

Benchmark
  commitId CommitId
  subject SubjectName
  deriving Show

Test
  benchmarkId BenchmarkId
  deriving Show

Factor
  testId TestId
  name Text
  value Text
  deriving Show
  deriving Eq
  deriving Ord

Metric
  testId TestId
  name MetricLabel
  rangeLower Double
  rangeUpper Double
  mean Double
  stddev Double
  deriving Show
|]

instance Ord Commit where
  compare c1 c2 =
   compare c1.commitCreatedAt c2.commitCreatedAt
   `mappend`
   compare c1.commitHash c2.commitHash
