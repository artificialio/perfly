module Perf.ImportSpec where

import Perf.Import
import Test.Hspec
import qualified Perf.Types.External as EX
import qualified Perf.Types.DB as DB
import Data.Aeson (decode)
import Data.Maybe (fromJust, listToMaybe, isNothing)
import Data.Text (Text)
import Data.String (fromString)
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when, forM_)
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sortOn)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (for_)
import System.IO
import System.Directory (doesFileExist, removeFile)

-- Sample test commit JSON for testing
sampleCommitJson :: BL.ByteString
sampleCommitJson = BL.pack
  "{\"branch\":\"test-branch\",\"commit\":\"abc123\",\"result\":{\"benchmarks\":[{\"subject\":\"test-subject\",\"tests\":[{\"factors\":[{\"factor\":\"test-factor\",\"value\":\"123\"}],\"metrics\":[{\"mean\":1.5,\"metric\":\"time\",\"rangeLower\":1.0,\"rangeUpper\":2.0,\"stddev\":0.1}]}]}]}}"

-- Complex test commit JSON with multiple benchmarks, tests, factors, and metrics
complexCommitJson :: BL.ByteString
complexCommitJson = BL.pack
  "{\"branch\":\"complex-branch\",\"commit\":\"def456\",\"result\":{\"benchmarks\":[{\"subject\":\"benchmark-1\",\"tests\":[{\"factors\":[{\"factor\":\"factor-1\",\"value\":\"value-1\"},{\"factor\":\"factor-2\",\"value\":\"value-2\"}],\"metrics\":[{\"mean\":1.0,\"metric\":\"metric-1\",\"rangeLower\":0.5,\"rangeUpper\":1.5,\"stddev\":0.2},{\"mean\":2.0,\"metric\":\"metric-2\",\"rangeLower\":1.5,\"rangeUpper\":2.5,\"stddev\":0.3}]}]},{\"subject\":\"benchmark-2\",\"tests\":[{\"factors\":[{\"factor\":\"factor-3\",\"value\":\"value-3\"}],\"metrics\":[{\"mean\":3.0,\"metric\":\"metric-3\",\"rangeLower\":2.5,\"rangeUpper\":3.5,\"stddev\":0.4}]}]}]}}"

spec :: Spec
spec = do
  describe "importCommit" $ do
    it "imports commit data into the database" $ do
      -- Parse the sample JSON to get our test commit
      let commit = fromJust $ decode sampleCommitJson :: EX.Commit

      -- Run the test with in-memory SQLite
      runSqlite @IO ":memory:" $ do
        -- Run migrations to create the schema
        runMigration DB.migrateAll

        -- Import the commit
        importCommit commit

        -- Verify the database state
        -- 1. Check Branch was created
        branches <- selectList [] []
        liftIO $ length branches `shouldBe` 1
        let branch = entityVal (head branches)
        liftIO $ DB.branchName branch `shouldBe` "test-branch"

        -- 2. Check Commit was created
        commits <- selectList [] []
        liftIO $ length commits `shouldBe` 1
        let commit' = entityVal (head commits)
        liftIO $ DB.commitHash commit' `shouldBe` "abc123"

        -- 3. Check Branch-Commit mapping
        branchCommits <- selectList [] [Asc DB.MapBranchCommitBranchId]
        liftIO $ length branchCommits `shouldBe` 1

        -- 4. Check Benchmark was created
        benchmarks <- selectList [] [Asc DB.BenchmarkCommitId]
        liftIO $ length benchmarks `shouldBe` 1
        let benchmark = entityVal (head benchmarks)
        liftIO $ DB.benchmarkSubject benchmark `shouldBe` "test-subject"

        -- 5. Check Test was created
        tests <- selectList [] [Asc DB.TestBenchmarkId]
        liftIO $ length tests `shouldBe` 1

        -- 6. Check Factor was created
        factors <- selectList [] [Asc DB.FactorTestId]
        liftIO $ length factors `shouldBe` 1
        let factor = entityVal (head factors)
        liftIO $ DB.factorName factor `shouldBe` "test-factor"
        liftIO $ DB.factorValue factor `shouldBe` "123"

        -- 7. Check Metric was created
        metrics <- selectList [] [Asc DB.MetricTestId]
        liftIO $ length metrics `shouldBe` 1
        let metric = entityVal (head metrics)
        liftIO $ DB.metricName metric `shouldBe` "time"
        liftIO $ DB.metricMean metric `shouldBe` 1.5
        liftIO $ DB.metricRangeLower metric `shouldBe` 1.0
        liftIO $ DB.metricRangeUpper metric `shouldBe` 2.0
        liftIO $ DB.metricStddev metric `shouldBe` 0.1

    it "handles duplicate commits correctly" $ do
      -- Parse the sample JSON to get our test commit
      let commit = fromJust $ decode sampleCommitJson :: EX.Commit

      -- Run the test with in-memory SQLite
      runSqlite @IO ":memory:" $ do

        -- Run migrations to create the schema
        runMigration DB.migrateAll

        -- Import the commit twice
        importCommit commit
        importCommit commit

        -- Verify that we only have one commit in the database
        commits :: [Entity DB.Commit] <- selectList [] []
        liftIO $ length commits `shouldBe` 1

        -- Verify we only have one benchmark (no duplicates)
        benchmarks :: [Entity DB.Benchmark] <- selectList [] []
        liftIO $ length benchmarks `shouldBe` 1

    it "can import from a file and query the database" $ do
      let testFile = "test-commit.json"

      -- Create a test file with our JSON
      liftIO $ BL.writeFile testFile sampleCommitJson

      -- Run the test with in-memory SQLite
      runSqlite @IO ":memory:" $ do
        -- Run migrations
        runMigration DB.migrateAll

        -- Read the file and import the commit
        jsonData <- liftIO $ BL.readFile testFile
        let commit = fromJust $ decode jsonData :: EX.Commit
        importCommit commit

        -- Query for all branches
        branches <- selectList [] []
        liftIO $ length branches `shouldBe` 1

        -- Query for all commits in the test-branch
        let branchId = entityKey (head branches)
        branchCommits <- selectList [DB.MapBranchCommitBranchId ==. branchId] []
        liftIO $ length branchCommits `shouldBe` 1

        -- Get the commit ID
        let commitId = DB.mapBranchCommitCommitId (entityVal (head branchCommits))

        -- Query for all benchmarks for this commit
        benchmarks <- selectList [DB.BenchmarkCommitId ==. commitId] []
        liftIO $ length benchmarks `shouldBe` 1

        -- Get the benchmark ID
        let benchmarkId = entityKey (head benchmarks)

        -- Query for all tests for this benchmark
        tests <- selectList [DB.TestBenchmarkId ==. benchmarkId] []
        liftIO $ length tests `shouldBe` 1

        -- Get the test ID
        let testId = entityKey (head tests)

        -- Query for all metrics for this test
        metrics <- selectList [DB.MetricTestId ==. testId] []
        liftIO $ length metrics `shouldBe` 1
        let metric = entityVal (head metrics)
        liftIO $ DB.metricName metric `shouldBe` "time"

        -- Query for all factors for this test
        factors <- selectList [DB.FactorTestId ==. testId] []
        liftIO $ length factors `shouldBe` 1
        let factor = entityVal (head factors)
        liftIO $ DB.factorName factor `shouldBe` "test-factor"

      -- Clean up the test file
      fileExists <- liftIO $ doesFileExist testFile
      when fileExists $ liftIO $ removeFile testFile

    it "correctly imports complex commit data with multiple benchmarks, tests, factors, and metrics" $ do
      -- Parse the complex JSON to get our test commit
      let commit = fromJust $ decode complexCommitJson :: EX.Commit

      -- Run the test with in-memory SQLite
      runSqlite @IO ":memory:" $ do
        -- Run migrations to create the schema
        runMigration DB.migrateAll

        -- Import the commit
        importCommit commit

        -- Verify the branch and commit
        branches <- selectList [DB.BranchName ==. "complex-branch"] []
        liftIO $ length branches `shouldBe` 1

        commits <- selectList [DB.CommitHash ==. "def456"] []
        liftIO $ length commits `shouldBe` 1
        let commitId = entityKey (head commits)

        -- Verify benchmarks (should be 2)
        benchmarks <- selectList [DB.BenchmarkCommitId ==. commitId] []
        liftIO $ length benchmarks `shouldBe` 2

        -- Get benchmarks by subject
        benchmark1 <- selectList [DB.BenchmarkCommitId ==. commitId, DB.BenchmarkSubject ==. "benchmark-1"] []
        liftIO $ length benchmark1 `shouldBe` 1
        let benchmark1Id = entityKey (head benchmark1)

        benchmark2 <- selectList [DB.BenchmarkCommitId ==. commitId, DB.BenchmarkSubject ==. "benchmark-2"] []
        liftIO $ length benchmark2 `shouldBe` 1
        let benchmark2Id = entityKey (head benchmark2)

        -- Verify tests
        tests1 <- selectList [DB.TestBenchmarkId ==. benchmark1Id] []
        liftIO $ length tests1 `shouldBe` 1
        let test1Id = entityKey (head tests1)

        tests2 <- selectList [DB.TestBenchmarkId ==. benchmark2Id] []
        liftIO $ length tests2 `shouldBe` 1
        let test2Id = entityKey (head tests2)

        -- Verify factors for test 1 (should be 2)
        factors1 <- selectList [DB.FactorTestId ==. test1Id] []
        liftIO $ length factors1 `shouldBe` 2

        -- Verify specific factors
        factor1 <- selectList [DB.FactorTestId ==. test1Id, DB.FactorName ==. "factor-1"] []
        liftIO $ length factor1 `shouldBe` 1
        liftIO $ DB.factorValue (entityVal (head factor1)) `shouldBe` "value-1"

        factor2 <- selectList [DB.FactorTestId ==. test1Id, DB.FactorName ==. "factor-2"] []
        liftIO $ length factor2 `shouldBe` 1
        liftIO $ DB.factorValue (entityVal (head factor2)) `shouldBe` "value-2"

        -- Verify factors for test 2 (should be 1)
        factors2 <- selectList [DB.FactorTestId ==. test2Id] []
        liftIO $ length factors2 `shouldBe` 1

        -- Verify specific factor
        factor3 <- selectList [DB.FactorTestId ==. test2Id, DB.FactorName ==. "factor-3"] []
        liftIO $ length factor3 `shouldBe` 1
        liftIO $ DB.factorValue (entityVal (head factor3)) `shouldBe` "value-3"

        -- Verify metrics for test 1 (should be 2)
        metrics1 <- selectList [DB.MetricTestId ==. test1Id] []
        liftIO $ length metrics1 `shouldBe` 2

        -- Verify specific metrics
        metric1 <- selectList [DB.MetricTestId ==. test1Id, DB.MetricName ==. "metric-1"] []
        liftIO $ length metric1 `shouldBe` 1
        let m1 = entityVal (head metric1)
        liftIO $ DB.metricMean m1 `shouldBe` 1.0
        liftIO $ DB.metricRangeLower m1 `shouldBe` 0.5
        liftIO $ DB.metricRangeUpper m1 `shouldBe` 1.5
        liftIO $ DB.metricStddev m1 `shouldBe` 0.2

        metric2 <- selectList [DB.MetricTestId ==. test1Id, DB.MetricName ==. "metric-2"] []
        liftIO $ length metric2 `shouldBe` 1
        let m2 = entityVal (head metric2)
        liftIO $ DB.metricMean m2 `shouldBe` 2.0

        -- Verify metrics for test 2 (should be 1)
        metrics2 <- selectList [DB.MetricTestId ==. test2Id] []
        liftIO $ length metrics2 `shouldBe` 1

        -- Verify specific metric
        metric3 <- selectList [DB.MetricTestId ==. test2Id, DB.MetricName ==. "metric-3"] []
        liftIO $ length metric3 `shouldBe` 1
        let m3 = entityVal (head metric3)
        liftIO $ DB.metricMean m3 `shouldBe` 3.0
