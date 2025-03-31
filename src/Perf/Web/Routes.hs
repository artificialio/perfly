module Perf.Web.Routes where

import Text.Printf
import qualified Data.Containers.ListUtils as List
import qualified Data.List as List
import qualified Data.Text as T
import Data.Traversable
import Perf.Web.Layout
import Yesod.Lucid
import Data.Maybe
import Perf.Web.Db
import Perf.Web.Foundation
import Perf.Web.Types
import Data.Text (Text)
import qualified Perf.Types.DB as DB
import qualified Perf.Types.External as EX
import Perf.Import
import Database.Persist.Sqlite
import RIO qualified
import Data.Foldable
import Yesod hiding (toHtml)

getHomeR :: Handler (Yesod.Lucid.Html ())
getHomeR = do
  branches <- db $ selectList @DB.Branch [] []
  lucid do
    defaultLayout_ do
      h1_ "All branches"
      for_ branches \(Entity _ branch) ->
        div_ do
          url <- asks (.url)
          a_ [href_ $ url $ BranchR branch.branchName] $
           toHtml $ branch.branchName

getBranchR :: Text -> Handler (Yesod.Lucid.Html ())
getBranchR name = do
  mbranch <- db $ selectFirst [DB.BranchName ==. name] []
  case mbranch of
    Nothing -> notFound
    Just (Entity branchId branch) -> do
      mappings <- db $ selectList [DB.MapBranchCommitBranchId ==. branchId] []
      commits <- fmap catMaybes $ RIO.for mappings \mapping ->
        db $ selectFirst [DB.CommitId ==. mapping.entityVal.mapBranchCommitCommitId] []
      lucid do
        defaultLayout_ do
          h1_ $ toHtml $ branch.branchName
          for_ commits \(Entity _ commit) ->
            div_ do
              url <- asks (.url)
              a_ [href_ $ url $ BranchCommitR name commit.commitHash] $
                toHtml $ commit.commitHash

getCommitR :: Text -> Handler (Yesod.Lucid.Html ())
getCommitR hash = do
  mcommit <- db $ selectFirst [DB.CommitHash ==. hash] []
  case mcommit of
    Nothing -> notFound
    Just (Entity commitId commit) -> do
      benchmarks0 <- db $ selectList [DB.BenchmarkCommitId ==. commitId] []
      benchmarks <- for benchmarks0 \(Entity benchmarkId benchmark) -> do
        tests0 <- db $ selectList [DB.TestBenchmarkId ==. benchmarkId] []
        tests <- for tests0 \(Entity testId _) -> do
          factors <- db $ selectList [DB.FactorTestId ==. testId] []
          metrics <- db $ selectList [DB.MetricTestId ==. testId] []
          pure (factors, metrics)
        pure (benchmark, tests)
      lucid do
        defaultLayout_ do
          h1_ $
            toHtml $ commit.commitHash
          generateTable benchmarks

generateTable :: [(DB.Benchmark, [([Entity DB.Factor], [Entity DB.Metric])])] -> HtmlT (Reader (Page App)) ()
generateTable benchmarks =
  div_ do
    forM_ benchmarks $ \(benchmark, tests) -> do
      let subject = benchmark.benchmarkSubject
      h3_ $ toHtml subject
      table_ do
        thead_ do
          tr_ do
            th_ "Factor"
            let headings =
                  List.nubOrd $
                  List.concatMap
                    (\(_factors, metrics) -> List.map (.entityVal.metricName) metrics)
                    tests
            for_ headings $ \factor ->
               th_ $ toHtml factor
        tbody_ do
          forM_ tests $ \(factors, metrics) -> do
            tr_ do
              td_ do
                forM_ factors $ \(Entity _ factor) -> do
                  let factorName = factor.factorName
                  let factorValue = factor.factorValue
                  div_ $ toHtml $ T.concat [T.strip factorName, "=", T.strip factorValue]
              forM_ metrics $ \(Entity _ metric) -> do
                let mean = metric.metricMean
                let stddev = metric.metricStddev
                let rangeUpper = metric.metricRangeUpper
                let rangeLower = metric.metricRangeLower
                td_ do
                  div_ do
                    "mean="
                    strong_ $ toHtml $ shortNum mean
                  div_ $ toHtml $ T.concat [
                    "(stddev=", shortNum stddev,
                    ", min..max=",
                    shortNum rangeLower, "..",
                    shortNum rangeUpper,
                    ")"
                    ]

shortNum :: Double -> Text
shortNum = T.pack . printf @(Double -> String) "%.3f"

getBranchCommitR :: Text -> Text -> Handler (Yesod.Lucid.Html ())
getBranchCommitR branch hash = do
  mbranch <- db $ selectFirst [DB.BranchName ==. branch] []
  case mbranch of
    Nothing -> notFound
    Just (Entity branchId _) -> do
      mcommit <- db $ selectFirst [DB.CommitHash ==. hash] []
      case mcommit of
        Nothing -> notFound
        Just (Entity commitId _) -> do
          isMapped <- db $ selectFirst [DB.MapBranchCommitBranchId ==. branchId,
                                        DB.MapBranchCommitCommitId ==. commitId] []
          case isMapped of
            Nothing -> notFound
            Just {} -> getCommitR hash

postReceiverR :: Handler ()
postReceiverR = do
  token <- fmap (.token) getYesod
  params <- reqGetParams <$> getRequest
  case List.lookup "token" params of
    Nothing -> invalidArgs ["No token."]
    Just given
      | given == token -> do
        commit :: EX.Commit <- requireCheckJsonBody
        db $ importCommit commit
      | otherwise -> permissionDenied "Bad token."
