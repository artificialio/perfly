module Perf.DB.Materialize where

import Data.Maybe (mapMaybe)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import Database.Persist
import Database.Persist.Sql (Single (..), rawSql)
import qualified Perf.Types.Prim as Prim
import qualified Perf.Types.DB as DB
import qualified Perf.Types.External as EX

type BenchmarkSeries key metric =
  Map Prim.SubjectName
    (Map (Set Prim.GeneralFactor)
      (Map Prim.MetricLabel
        (Map key metric)))

data DisplayMetric = DisplayMetric
  { mean :: Double,
    stddev :: Double
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
      let commitIds = map (.entityVal.mapBranchCommitCommitId) mappings
      commits <- selectList [DB.CommitId <-. commitIds] []
      let commitsById = Map.fromList [(entityKey commit, commit) | commit <- commits]
      pure $ reverse $ mapMaybe (`Map.lookup` commitsById) commitIds

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

-- | Loaded plot series for a branch page / CLI report.
data BranchPlotData = BranchPlotData
  { masterCommits :: [DB.Commit],
    -- ^ Master commits in historical order (oldest first).
    branchCommits :: [DB.Commit],
    -- ^ Current-branch commits in historical order; empty when viewing master.
    benchmarks :: BenchmarkSeries DB.Commit DB.Metric,
    tableCommitEntities :: [Entity DB.Commit]
    -- ^ Commits for the branch commits table (historical order).
  }

-- | Load master + branch series for plotting.
--
-- When @branchName == "master"@, @branchCommits@ is empty and @benchmarks@
-- contains only master data. Otherwise master metrics are merged into branch
-- subjects (master-only subjects are dropped).
loadBranchPlotData ::
  Text ->
  Int ->
  Int ->
  DB.DB (Maybe BranchPlotData)
loadBranchPlotData branchName branchLimit masterLimit = do
  let isMaster = branchName == "master"
  masterEntities <- loadBranchCommits "master" masterLimit
  branchEntities <-
    if isMaster
      then pure []
      else loadBranchCommits branchName branchLimit
  let masterCommits = map entityVal masterEntities
      branchCommits = map entityVal branchEntities
      tableCommitEntities = if isMaster then masterEntities else branchEntities
  case (isMaster, NonEmpty.nonEmpty masterEntities, NonEmpty.nonEmpty branchEntities) of
    (True, Nothing, _) -> pure Nothing
    (True, Just masterNE, _) -> do
      benchmarks <- materializeCommits masterNE
      pure $
        Just
          BranchPlotData
            { masterCommits,
              branchCommits = [],
              benchmarks,
              tableCommitEntities
            }
    (False, _, Nothing) -> pure Nothing
    (False, Nothing, Just branchNE) -> do
      benchmarks <- materializeCommits branchNE
      pure $
        Just
          BranchPlotData
            { masterCommits,
              branchCommits,
              benchmarks,
              tableCommitEntities
            }
    (False, Just masterNE, Just branchNE) -> do
      masterBenchmarks <- materializeCommits masterNE
      branchBenchmarks <- materializeCommits branchNE
      pure $
        Just
          BranchPlotData
            { masterCommits,
              branchCommits,
              benchmarks = mergeMasterIntoBranch masterBenchmarks branchBenchmarks,
              tableCommitEntities
            }

-- Materialize a set of commits into a data set we can work with.
materializeCommits ::
  NonEmpty (Entity DB.Commit) ->
  DB.DB
    (BenchmarkSeries DB.Commit DB.Metric)
materializeCommits commits = do
  let commitsById = Map.fromList [(entityKey commit, entityVal commit) | commit <- NonEmpty.toList commits]
      commitIds = Map.keys commitsById
      params = map toPersistValue commitIds
      joinFor table =
        " JOIN test ON "
          <> table
          <> ".test_id = test.id JOIN benchmark ON test.benchmark_id = benchmark.id WHERE benchmark.commit_id IN ("
          <> T.intercalate "," (map (const "?") commitIds)
          <> ")"
  factorRows :: [(Single DB.TestId, Single Text, Single Text)] <-
    rawSql ("SELECT factor.test_id, factor.name, factor.value FROM factor" <> joinFor "factor") params
  metricRows :: [(Entity DB.Metric, Single DB.CommitId, Single Prim.SubjectName)] <-
    rawSql ("SELECT ??, benchmark.commit_id, benchmark.subject FROM metric" <> joinFor "metric") params
  let factorsByTest =
        Map.fromListWith
          Set.union
          [ (testId, Set.singleton (Prim.GeneralFactor name value))
          | (Single testId, Single name, Single value) <- factorRows
          ]
  pure $
    Map.fromListWith (Map.unionWith (Map.unionWith Map.union))
      [ ( subject,
          Map.singleton
            (Map.findWithDefault Set.empty metric.metricTestId factorsByTest)
            (Map.singleton metric.metricName (Map.singleton commit metric))
        )
      | (Entity _ metric, Single commitId, Single subject) <- metricRows,
        Just commit <- [Map.lookup commitId commitsById]
      ]

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
                          ( Prim.MetricLabel metric.metric,
                            Map.singleton
                              label
                              DisplayMetric
                                { mean = metric.mean,
                                  stddev = metric.stddev
                                }
                          )
                 in (factors, metrics))
