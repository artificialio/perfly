module Perf.Import where

import Control.Monad
import Data.Maybe
import Data.Functor
import Data.Foldable
import qualified Perf.Types.External as EX
import Database.Persist
import qualified Perf.Types.DB as DB
import Data.Time
import Database.Persist.Sqlite
import Control.Monad.IO.Class

-- Import from an external commit into the database.
importCommit :: EX.Commit -> DB.DB ()
importCommit commit = do
  now <- liftIO getCurrentTime
  -- Ensure that the branch exists
  Entity branchId _ <- upsert DB.Branch {
     branchName = commit.branch,
     branchCreatedAt = now
   } []
  -- Check whether the commit already exists.
  mcommit <- fmap listToMaybe $ selectList [DB.CommitHash ==. commit.commit] []
  -- Ensure it does exist.
  commitId <- case mcommit of
    Just commit -> pure commit.entityKey
    Nothing -> insert DB.Commit {
        commitHash = commit.commit,
        commitCreatedAt = now
      }
  -- Add the commit to the branch if needed.
  void $ insertUnique DB.MapBranchCommit {
      mapBranchCommitBranchId = branchId,
      mapBranchCommitCommitId = commitId
      }
  -- If we're dealing with a new commit, populate the
  -- benchmarks. Otherwise, do nothing.
  when (isNothing mcommit) do
    -- Import the benchmarks into the commit
    for_ commit.result.benchmarks \benchmark -> do
      benchmarkId <- insert DB.Benchmark {
          benchmarkCommitId = commitId,
          benchmarkSubject = benchmark.subject
        }
      for_ benchmark.tests \test -> do
        testId <- insert DB.Test {
           testBenchmarkId = benchmarkId
         }
        for_ test.factors \factor ->
          insert_ DB.Factor {
            factorTestId = testId,
            factorName = factor.factor,
            factorValue = factor.value
          }
        for_ test.metrics \metric ->
          insert_ DB.Metric { metricTestId = testId,
            metricName = metric.metric,
               metricRangeLower = metric.rangeLower,
               metricRangeUpper = metric.rangeUpper,
               metricMean = metric.mean,
               metricStddev = metric.stddev
             }
