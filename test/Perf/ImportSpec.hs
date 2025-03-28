module Perf.ImportSpec where

import Test.Hspec.Expectations.Lifted
import Perf.Import
import Test.Hspec (Spec, it)
import qualified Perf.Types.External as EX
import qualified Perf.Types.DB as DB
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Control.Monad
import Data.Time
import Data.List
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import System.IO

sampleCommit1 :: L.ByteString
sampleCommit1 = "{\"branch\":\"master\",\"commit\":\"997d6b8c3cf3c5ea14c40ec8c0f55f9c574a51cc\",\"result\":{\"benchmarks\":[{\"subject\":\"policy addition benchmark\",\"tests\":[{\"factors\":[{\"factor\":\"policies\",\"value\":\"2\"}],\"metrics\":[{\"mean\":0.1,\"metric\":\"time\",\"rangeLower\":0.1,\"rangeUpper\":1.1,\"stddev\":0.1},{\"mean\":610.1,\"metric\":\"allocated MiB\",\"rangeLower\":260.1,\"rangeUpper\":1660.1,\"stddev\":699.1},{\"mean\":433.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":433.1,\"rangeUpper\":433.1,\"stddev\":0.1}]},{\"factors\":[{\"factor\":\"policies\",\"value\":\"20\"}],\"metrics\":[{\"mean\":4.1,\"metric\":\"time\",\"rangeLower\":4.1,\"rangeUpper\":4.1,\"stddev\":0.1},{\"mean\":2605.1,\"metric\":\"allocated MiB\",\"rangeLower\":2604.1,\"rangeUpper\":2606.1,\"stddev\":0.1},{\"mean\":471.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":444.1,\"rangeUpper\":489.1,\"stddev\":19.1}]},{\"factors\":[{\"factor\":\"policies\",\"value\":\"200\"}],\"metrics\":[{\"mean\":41.1,\"metric\":\"time\",\"rangeLower\":41.1,\"rangeUpper\":41.1,\"stddev\":0.1},{\"mean\":26074.1,\"metric\":\"allocated MiB\",\"rangeLower\":26074.1,\"rangeUpper\":26074.1,\"stddev\":0.1},{\"mean\":499.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":493.1,\"rangeUpper\":505.1,\"stddev\":5.1}]}]},{\"subject\":\"Marine Hull: transactPolicy\",\"tests\":[{\"factors\":[{\"factor\":\"iterations\",\"value\":\"20\"},{\"factor\":\"datapoints\",\"value\":\"1\"}],\"metrics\":[{\"mean\":2.1,\"metric\":\"time\",\"rangeLower\":2.1,\"rangeUpper\":2.1,\"stddev\":0.1},{\"mean\":2037.1,\"metric\":\"allocated MiB\",\"rangeLower\":2036.1,\"rangeUpper\":2038.1,\"stddev\":0.1},{\"mean\":1674.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":688.1,\"rangeUpper\":2700.1,\"stddev\":633.1}]}]}]}}"

sampleCommit2 :: L.ByteString
sampleCommit2 = "{\"branch\":\"master\",\"commit\":\"cc15a475c9f55f0c8ce04c41ae5c3fc3c8b6d799\",\"result\":{\"benchmarks\":[{\"subject\":\"policy addition benchmark\",\"tests\":[{\"factors\":[{\"factor\":\"policies\",\"value\":\"2\"}],\"metrics\":[{\"mean\":0.1,\"metric\":\"time\",\"rangeLower\":0.1,\"rangeUpper\":1.1,\"stddev\":0.1},{\"mean\":610.1,\"metric\":\"allocated MiB\",\"rangeLower\":260.1,\"rangeUpper\":1660.1,\"stddev\":699.1},{\"mean\":433.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":433.1,\"rangeUpper\":433.1,\"stddev\":0.1}]},{\"factors\":[{\"factor\":\"policies\",\"value\":\"20\"}],\"metrics\":[{\"mean\":4.1,\"metric\":\"time\",\"rangeLower\":4.1,\"rangeUpper\":4.1,\"stddev\":0.1},{\"mean\":2605.1,\"metric\":\"allocated MiB\",\"rangeLower\":2604.1,\"rangeUpper\":2606.1,\"stddev\":0.1},{\"mean\":471.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":444.1,\"rangeUpper\":489.1,\"stddev\":19.1}]},{\"factors\":[{\"factor\":\"policies\",\"value\":\"200\"}],\"metrics\":[{\"mean\":41.1,\"metric\":\"time\",\"rangeLower\":41.1,\"rangeUpper\":41.1,\"stddev\":0.1},{\"mean\":26074.1,\"metric\":\"allocated MiB\",\"rangeLower\":26074.1,\"rangeUpper\":26074.1,\"stddev\":0.1},{\"mean\":499.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":493.1,\"rangeUpper\":505.1,\"stddev\":5.1}]}]},{\"subject\":\"Marine Hull: transactPolicy\",\"tests\":[{\"factors\":[{\"factor\":\"iterations\",\"value\":\"20\"},{\"factor\":\"datapoints\",\"value\":\"1\"}],\"metrics\":[{\"mean\":2.1,\"metric\":\"time\",\"rangeLower\":2.1,\"rangeUpper\":2.1,\"stddev\":0.1},{\"mean\":2037.1,\"metric\":\"allocated MiB\",\"rangeLower\":2036.1,\"rangeUpper\":2038.1,\"stddev\":0.1},{\"mean\":1674.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":688.1,\"rangeUpper\":2700.1,\"stddev\":633.1}]}]}]}}"

spec :: Spec
spec = do
  it "importCommit" do
    runSqlite @IO ":memory:" do
      runMigrationQuiet DB.migrateAll
      ------------------------------------------
      -- Import first commit
      importCommit $ either error id $ eitherDecode @EX.Commit sampleCommit1
      importCommit $ either error id $ eitherDecode @EX.Commit sampleCommit1
      -- Check branches
      branches <- selectList @DB.Branch [] []
      shouldBe (map (.entityVal.branchName) branches) ["master"]
      -- Check commits
      commits <- selectList @DB.Commit [] []
      shouldBe (map (.entityVal.commitHash) commits) ["997d6b8c3cf3c5ea14c40ec8c0f55f9c574a51cc"]
      -- Check branch<->commit mapping
      maps <- selectList @DB.MapBranchCommit [] []
      shouldBe (map entityVal maps)
        [DB.MapBranchCommit branch.entityKey commit.entityKey
        | branch <- branches, commit <- commits]

      ------------------------------------------
      -- Import another commit
      importCommit $ either error id $ eitherDecode @EX.Commit sampleCommit2
      importCommit $ either error id $ eitherDecode @EX.Commit sampleCommit2
      -- Check branches idempotency
      branches <- selectList @DB.Branch [] []
      shouldBe (map (.entityVal.branchName) branches) ["master"]
      -- Check commits
      commits' <- selectList @DB.Commit [] [Asc DB.CommitId]
      shouldBe (map (.entityVal.commitHash) commits')
        ["997d6b8c3cf3c5ea14c40ec8c0f55f9c574a51cc"
        ,"cc15a475c9f55f0c8ce04c41ae5c3fc3c8b6d799"]
      -- Check branch<->commit mapping
      maps <- selectList @DB.MapBranchCommit [] [Asc DB.MapBranchCommitId]
      shouldSatisfy maps $ (==2) . length
      shouldBe (map entityVal maps)
        [DB.MapBranchCommit branch.entityKey commit.entityKey
        | branch <- branches, commit <- commits']

      ------------------------------------------
      -- Sanity checks, there's nothing clever but insertion going on
      -- here.
      benchmarks <- selectList @DB.Benchmark [] []
      shouldSatisfy benchmarks $ (==4) . length
      tests <- selectList @DB.Test [] []
      shouldSatisfy tests $ (==8) . length
      factors <- selectList @DB.Factor [] []
      shouldSatisfy factors $ (==10) . length
      metrics <- selectList @DB.Metric [] []
      shouldSatisfy metrics $ (==24) . length
