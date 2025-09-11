module Perf.DB.Materialize where

import qualified Data.List as List
import Data.Traversable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Perf.Types.Prim as Prim
import Database.Persist
import qualified Perf.Types.DB as DB

-- Materialize a set of commits into a data set we can work with.
materializeCommits ::
  NonEmpty (Entity DB.Commit) ->
  DB.DB
    (Map Prim.SubjectName
      (Map (Set Prim.GeneralFactor)
        (Map Prim.MetricLabel
           (Map DB.Commit DB.Metric))))
materializeCommits commits = do
  benchmarks <- traverse materializeCommit commits
  pure $
    List.foldl1' (Map.unionWith (Map.unionWith (Map.unionWith Map.union))) $
    NonEmpty.toList $
    fmap (fmap (fmap (fmap (\(commit, metric) -> Map.singleton commit metric)))) $
    benchmarks

-- Materialize a commit at a given timestamp into data we can work with.
materializeCommit ::
  Entity DB.Commit ->
  DB.DB
    (Map Prim.SubjectName
      (Map (Set Prim.GeneralFactor)
        (Map Prim.MetricLabel
           (DB.Commit, DB.Metric))))
materializeCommit commit = do
  benchmarks0 <- selectList [DB.BenchmarkCommitId ==. commit.entityKey] [Desc DB.BenchmarkCommitId]
  benchmarks <- for benchmarks0 \(Entity benchmarkId benchmark) -> do
    tests0 <- selectList [DB.TestBenchmarkId ==. benchmarkId] []
    tests <- for tests0 \(Entity testId _) -> do
      factors <- selectList [DB.FactorTestId ==. testId] []
      metrics <- selectList [DB.MetricTestId ==. testId] []
      pure (map (\factor -> Prim.GeneralFactor factor.entityVal.factorName
                                               factor.entityVal.factorValue)
                factors,
            map (.entityVal) metrics)
    pure (benchmark, tests)
  pure $ Map.fromList $
    flip map benchmarks \(benchmark, tests) ->
      (benchmark.benchmarkSubject,
        Map.fromList $ flip map tests \(factors, metrics) ->
          (Set.fromList factors,
           Map.fromList $
             flip map metrics \metric ->
               (metric.metricName,
                 (commit.entityVal, metric)))
        )
