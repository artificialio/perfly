module Perf.DB.Materialize where

import Data.Maybe (catMaybes)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Traversable
import Database.Persist
import qualified Perf.Types.Prim as Prim
import qualified Perf.Types.DB as DB
import qualified Perf.Types.External as EX

type BenchmarkSeries key metric =
  Map Prim.SubjectName
    (Map (Set Prim.GeneralFactor)
      (Map Prim.MetricLabel
        (Map key metric)))

data DisplayMetric = DisplayMetric
  { mean :: Double
  }
  deriving (Eq, Show)

-- | Most recent commits on a branch, returned in historical order (oldest first).
loadBranchCommits :: Text -> Int -> DB.DB [Entity DB.Commit]
loadBranchCommits branchName limit = do
  mbranch <- selectFirst [DB.BranchName ==. branchName] []
  case mbranch of
    Nothing -> pure []
    Just (Entity branchId _) -> do
      mappings <-
        selectList
          [DB.MapBranchCommitBranchId ==. branchId]
          [Desc DB.MapBranchCommitId, LimitTo limit]
      commits <-
        mapM
          (\mapping -> selectFirst [DB.CommitId ==. mapping.entityVal.mapBranchCommitCommitId] [])
          mappings
      pure $ reverse $ catMaybes commits

-- | Merge master metrics into branch subjects only (drop master-only subjects).
mergeMasterIntoBranch ::
  Ord key =>
  BenchmarkSeries key metric ->
  BenchmarkSeries key metric ->
  BenchmarkSeries key metric
mergeMasterIntoBranch masterBenchmarks branchBenchmarks =
  Map.mapWithKey
    ( \subject branchTests ->
        case Map.lookup subject masterBenchmarks of
          Nothing -> branchTests
          Just masterTests ->
            Map.unionWith (Map.unionWith Map.union) masterTests branchTests
    )
    branchBenchmarks

-- Materialize a set of commits into a data set we can work with.
materializeCommits ::
  NonEmpty (Entity DB.Commit) ->
  DB.DB
    (BenchmarkSeries DB.Commit DB.Metric)
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

materializeExternalSnapshots ::
  NonEmpty (Text, [EX.Benchmark]) ->
  BenchmarkSeries Text DisplayMetric
materializeExternalSnapshots snapshots =
  List.foldl1' (Map.unionWith (Map.unionWith (Map.unionWith Map.union))) $
    NonEmpty.toList $
      fmap materializeSnapshot snapshots
  where
    materializeSnapshot :: (Text, [EX.Benchmark]) -> BenchmarkSeries Text DisplayMetric
    materializeSnapshot (label, benchmarks) =
      Map.fromList $
        flip map benchmarks \benchmark ->
          (Prim.SubjectName benchmark.subject,
            Map.fromList $
              flip map benchmark.tests \test ->
                let factors =
                      Set.fromList $
                        flip map test.factors \factor ->
                          Prim.GeneralFactor factor.factor factor.value
                    metrics =
                      Map.fromList $
                        flip map test.metrics \metric ->
                          (Prim.MetricLabel metric.metric, Map.singleton label DisplayMetric {mean = metric.mean})
                 in (factors, metrics))
