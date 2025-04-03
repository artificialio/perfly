module Perf.Web.Routes where

import Lucid.Base
import qualified Data.List as List
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Perf.Types.Prim as Prim
import Database.Persist
import qualified Perf.Types.DB as DB
import Data.Coerce
import Text.Printf
import qualified Data.Containers.ListUtils as List
import qualified Data.Text as T
import Perf.Web.Layout
import Yesod.Lucid
import Data.Maybe
import Perf.Web.Db
import Perf.Web.Foundation
import Perf.Types.Web
import Data.Text (Text)
import qualified Perf.Types.External as EX
import Perf.DB.Import
import Perf.DB.Materialize
import RIO qualified
import Data.Foldable
import Yesod hiding (toHtml, Html)

getHomeR :: Handler (Html ())
getHomeR = do
  master <- db $ selectList @DB.Branch [DB.BranchName ==. "master"] []
  branches <- db $ selectList @DB.Branch [DB.BranchName !=. "master"] [Desc DB.BranchCreatedAt]
  lucid do
    defaultLayout_ "Performance" do
      ul_ $
        for_ (master <> branches) \(Entity _ branch) ->
          li_ do
            url <- asks (.url)
            a_ [href_ $ url $ BranchR branch.branchName] $
             toHtml $ branch.branchName

getBranchR :: Text -> Handler (Html ())
getBranchR name = do
  mbranch <- db $ selectFirst [DB.BranchName ==. name] []
  case mbranch of
    Nothing -> notFound
    Just (Entity branchId branch) -> do
      mappings <- db $ selectList [DB.MapBranchCommitBranchId ==. branchId]
                                  [Desc DB.MapBranchCommitId]
      commits <- fmap catMaybes $ RIO.for mappings \mapping ->
        db $ selectFirst [DB.CommitId ==. mapping.entityVal.mapBranchCommitCommitId] []
      lucid do
        defaultLayout_ branch.branchName do
          table_ $
            for_ commits \(Entity _ commit) -> do
              url <- asks (.url)
              tr_ do
                td_ $
                  small_ $ toHtml $ show commit.commitCreatedAt
                td_ $
                  a_ [href_ $ url $ BranchCommitR name commit.commitHash] $
                    code_ $ toHtml $ commit.commitHash

getCommitR :: Prim.Hash -> Handler (Html ())
getCommitR hash = do
  mcommit <- db $ selectFirst [DB.CommitHash ==. hash] []
  case mcommit of
    Nothing -> notFound
    Just commit -> do
      benchmarks <- db $ materializeCommit commit
      lucid do
        defaultLayout_ (coerce commit.entityVal.commitHash) do
          generateTable generateSingleMetric benchmarks

generateTable ::
  (metric -> Html ()) ->
  (Map Prim.SubjectName
      (Map (Set DB.Factor)
        (Map Prim.MetricLabel
           metric)))
 -> HtmlT (Reader (Page App)) ()
generateTable generateMetric benchmarks =
  div_ do
    forM_ (Map.toList benchmarks) $ \(subject, tests) -> do
      h3_ $ toHtml subject
      table_ do
        thead_ do
          tr_ do
            th_ "Factor"
            let headings =
                  List.nubOrd $
                  List.concatMap
                    (\(_factors, metrics) -> Map.keys metrics)
                    $ Map.toList tests
            for_ headings $ \factor ->
               th_ $ toHtml factor
        tbody_ do
          forM_ (Map.toList tests) $ \(factors, metrics) -> do
            tr_ do
              td_ do
                forM_ factors $ \factor -> do
                  let factorName = factor.factorName
                  let factorValue = factor.factorValue
                  div_ $ toHtml $ T.concat [T.strip factorName, "=", T.strip factorValue]
              forM_ metrics $ generalizeHtmlT . generateMetric

generateSingleMetric :: (DB.Commit, DB.Metric) -> Html ()
generateSingleMetric (_, metric) = do
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

generatePluralMetric :: Map DB.Commit DB.Metric -> Html ()
generatePluralMetric _ = pure ()

shortNum :: Double -> Text
shortNum = T.pack . printf @(Double -> String) "%.3f"

getBranchCommitR :: Text -> Prim.Hash -> Handler (Html ())
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
