module Perf.Web.Routes where

import Data.Bifunctor
import Perf.Web.Chart
import Data.Traversable
import qualified Data.List.NonEmpty as NonEmpty
import Lucid.Base
import qualified Data.List as List
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
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
      let maxGraph :: Int = 28
      mappings <- db $ selectList [DB.MapBranchCommitBranchId ==. branchId]
                                  [Desc DB.MapBranchCommitId, LimitTo maxGraph]
      commits <- fmap catMaybes $ RIO.for mappings \mapping ->
        db $ selectFirst [DB.CommitId ==. mapping.entityVal.mapBranchCommitCommitId] []
      mbenchmarks <- db $ for (NonEmpty.nonEmpty $ reverse commits) materializeCommits
      lucid do
        defaultLayout_ branch.branchName do
          for_ mbenchmarks $ generalizeHtmlT . generatePlots
          h1_ "Commits"
          table_ do
            let prevCommits = map Just (drop 1 commits) <> repeat Nothing
            for_ (zip commits prevCommits) \(Entity _ commit, mprev) -> do
              let mprevious = fmap (.entityVal) mprev
              url <- asks (.url)
              tr_ do
                td_ $
                  small_ $ toHtml $ show commit.commitCreatedAt
                td_ $
                  a_ [href_ $ url $ BranchCommitR name commit.commitHash] $
                    code_ $ toHtml $ commit.commitHash
                td_ $
                  for_ mprevious \previous ->
                    a_ [href_ $ url $
                          CompareCommitsR previous.commitHash commit.commitHash] $
                      "compare previous"
          p_ $ small_ do
            "(limited to most recent "
            toHtml $ show maxGraph
            " commits)"

factorSmall :: Prim.GeneralFactor -> Text
factorSmall factor = T.concat [T.strip factor.name, "=", T.strip factor.value]

factorsSmall :: Set Prim.GeneralFactor -> Text
factorsSmall = T.intercalate "," . map factorSmall . toList

generatePlots ::
  (Map Prim.SubjectName
      (Map (Set Prim.GeneralFactor)
        (Map Prim.MetricLabel
           (Map DB.Commit DB.Metric))))
  -> Html ()
generatePlots benchmarks = do
  unless (Map.null benchmarks) $ h1_ "Plots"
  for_ (zip [0 :: Int ..] (Map.toList benchmarks)) \(b_i,(subject, tests)) -> do
    h2_ $ toHtml subject
    let labels :: Set DB.Commit =
          Set.fromList $ concatMap (concatMap Map.keys . Map.elems) $ Map.elems tests
    let metrics :: Set Prim.MetricLabel =
          Set.fromList $ concatMap Map.keys $ Map.elems tests
    -- Produce a chart for each type of metric.
    div_ [style_ "display: flex; flex-wrap: wrap;"] do
      for_ (zip [0 :: Int ..] (toList metrics)) \(m_i, metricLabel) -> do
        let dataSets =
              map (second (maybe [] Map.elems . Map.lookup metricLabel))
              $ Map.toList tests
        chart_ (T.pack (show b_i) <> "-" <> T.pack (show m_i)) $
          makeChartConfig metricLabel labels dataSets

makeChartConfig ::
  Prim.MetricLabel ->
  Set DB.Commit ->
  [(Set Prim.GeneralFactor, [DB.Metric])] ->
  Value
makeChartConfig metricName commits dataSets =
  object
  [ "type" .= ("line" :: Text)
  , "data" .= chartData
  , "options" .= object
      [ "responsive" .= True
      , "maintainAspectRatio" .= False
      , "animations" .= False
      , "plugins" .= object
          [ "title" .= object
              [ "display" .= True
              , "text" .= coerce @_ @Text metricName
              , "font" .= object
                  [ "size" .= (16 :: Int)
                  ]
              ]
          , "tooltip" .= object
              [ "callbacks" .= object
                  [ "title" .= ("function(tooltipItems) { return 'Commit: ' + tooltipItems[0].label; }" :: Text)
                  ]
              ]
          ]
      , "scales" .= object
          [ "x" .= object
              [ "title" .= object
                  [ "display" .= False
                  , "text" .= ("Commits" :: Text)
                  ]
              ]
          , "y" .= object
              [ "title" .= object
                  [ "display" .= True
                  , "text" .= coerce @_ @Text metricName
                  ]
              , "beginAtZero" .= True
              ]
          ]
      ]
  ]

  where
    chartData = object
      [ "labels" .= List.map (T.take 8 . (coerce :: Prim.Hash -> Text) . (.commitHash)) (Set.toList commits)
      , "datasets" .=
          [ object
              [ "label" .= factorsSmall factors
              , "data" .= (map (.metricMean) metrics :: [Double])
              , "borderColor" .= color
              , "tension" .= (0.1 :: Double)
              , "fill" .= False
              ]
          | ((factors, metrics), color) <- zip dataSets $ cycle colors
          ]
      ]
    colors :: [Text] = T.words
      "#4394E5 #87BB62 #876FD4 #F5921B"

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

getCompareCommitsR :: Prim.Hash ->  Prim.Hash -> Handler (Html ())
getCompareCommitsR before after = do
  mhash0 <- db $ selectFirst [DB.CommitHash ==. before] []
  mhash1 <- db $ selectFirst [DB.CommitHash ==. after] []
  case (,) <$> mhash0 <*> mhash1 of
    Nothing -> notFound
    Just (hash0,hash1) -> do
      benchmarks <- db $ materializeCommits $ hash0 NonEmpty.:| [hash1]
      lucid do
        defaultLayout_ "Compare commits" do
          generateTable generatePluralMetric benchmarks

generateTable ::
  (metric -> Html ()) ->
  (Map Prim.SubjectName
      (Map (Set Prim.GeneralFactor)
        (Map Prim.MetricLabel
           metric)))
 -> HtmlT (Reader (Page App)) ()
generateTable generateMetric benchmarks =
  div_ do
    forM_ (Map.toList benchmarks) $ \(subject, tests) -> do
      h3_ $ toHtml subject
      div_ [style_ "overflow-x: scroll;"] do 
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
                    let factorName = factor.name
                    let factorValue = factor.value
                    div_ $ toHtml $ T.concat [T.strip factorName, "=", T.strip factorValue]
                forM_ metrics $ generalizeHtmlT . generateMetric

generateSingleMetric :: (DB.Commit, DB.Metric) -> Html ()
generateSingleMetric (commit, metric) =
  generatePluralMetric $ Map.singleton commit metric

data MetricStyle = Bold | Small

generatePluralMetric :: Map DB.Commit DB.Metric -> Html ()
generatePluralMetric metrics = do
  td_ do
    div_ do
      table_ $ do
        property Bold "mean" (.metricMean)
        property Small "stddev" (.metricStddev)
        property Small "min" (.metricRangeLower)
        property Small "max" (.metricRangeUpper)
  where
    colorize color s = span_ [style_ ("color: " <> color)] s
    property style label accessor = do
      let textWrapper = case style of 
           Bold -> strong_ 
           Small -> small_
      tr_ $ do 
           td_ $ textWrapper $ em_ label 
           td_$ textWrapper $ do 
		let diff = foldl1 (-) $ map accessor $ Map.elems metrics
		if Map.size metrics > 1 && diff /= 0 then do
			sequence_ $
			  List.intersperse "-" $
			  map (shortNum . accessor) $
			  Map.elems metrics
			"="
			colorize "red" $ shortNum $ diff
		else
			sequence_ $
			  map (shortNum . accessor) $
			  take 1 $ Map.elems metrics

shortNum :: Double -> Html ()
shortNum =
  span_ [style_ "font-family: monospace"] .
  toHtml .
  T.pack .
  printf @(Double -> String) "%.3f"

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
