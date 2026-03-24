module Perf.Web.Plot where

import Data.Aeson
import Data.Coerce
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Control.Monad
import Lucid
import Perf.DB.Materialize
import Perf.Types.DB qualified as DB
import Perf.Types.Prim qualified as Prim
import Perf.Web.Chart

generateCommitPlots :: BenchmarkSeries DB.Commit DB.Metric -> Html ()
generateCommitPlots =
  generatePlotsWith
    (T.take 8 . (coerce :: Prim.Hash -> Text) . (.commitHash))
    (.metricMean)

generateExternalPlots :: BenchmarkSeries Text DisplayMetric -> Html ()
generateExternalPlots =
  generatePlotsWith id (.mean)

generatePlotsWith ::
  Ord key =>
  (key -> Text) ->
  (metric -> Double) ->
  BenchmarkSeries key metric ->
  Html ()
generatePlotsWith renderKey metricMean benchmarks = do
  unless (Map.null benchmarks) $ h1_ "Plots"
  Foldable.for_ (zip [0 :: Int ..] (Map.toList benchmarks)) \(benchmarkIdx, (subject, tests)) -> do
    h2_ $ toHtml subject
    let keys =
          Set.fromList $ concatMap (concatMap Map.keys . Map.elems) $ Map.elems tests
    let orderedKeys = Set.toList keys
    let labels = map renderKey orderedKeys
    let metrics :: Set Prim.MetricLabel =
          Set.fromList $ concatMap Map.keys $ Map.elems tests
    div_ [style_ "display: flex; flex-wrap: wrap;"] do
      Foldable.for_ (zip [0 :: Int ..] (Set.toList metrics)) \(metricIdx, metricLabel) -> do
        let dataSets =
              flip map (Map.toList tests) \(factors, allMetrics) ->
                (factors, toSeries orderedKeys (Map.findWithDefault Map.empty metricLabel allMetrics))
        let (plotData, layout) = makePlotlyConfig metricLabel labels dataSets
        chart_ (T.pack (show benchmarkIdx) <> "-" <> T.pack (show metricIdx)) plotData layout
  where
    toSeries orderedKeys metricMap =
      flip map orderedKeys \key ->
        maybe Null (toJSON . metricMean) $
          Map.lookup key metricMap

makePlotlyConfig ::
  Prim.MetricLabel ->
  [Text] ->
  [(Set Prim.GeneralFactor, [Value])] ->
  (Value, Value)
makePlotlyConfig metricName labels dataSets =
  (toJSON traces, layout)
  where
    traces =
      [ object
          [ "x" .= labels,
            "y" .= series,
            "type" .= ("scatter" :: Text),
            "mode" .= ("lines+markers" :: Text),
            "name" .= factorsSmall factors,
            "line" .= object ["color" .= color]
          ]
        | ((factors, series), color) <- zip dataSets $ cycle colors
      ]
    layout =
      object
        [ "title"
            .= object
              [ "text" .= coerce @_ @Text metricName,
                "font" .= object ["family" .= ("monospace" :: Text), "size" .= (16 :: Int)]
              ],
          "xaxis"
            .= object
              [ "title" .= ("" :: Text),
                "tickfont" .= object ["family" .= ("monospace" :: Text)]
              ],
          "yaxis"
            .= object
              [ "title" .= coerce @_ @Text metricName,
                "rangemode" .= ("tozero" :: Text),
                "tickfont" .= object ["family" .= ("monospace" :: Text)]
              ],
          "font" .= object ["family" .= ("monospace" :: Text)],
          "hovermode" .= ("x unified" :: Text),
          "showlegend" .= True,
          "legend" .= object ["x" .= (1 :: Int), "y" .= (0 :: Int), "xanchor" .= ("right" :: Text), "bgcolor" .= ("rgba(0,0,0,0)" :: Text), "font" .= object ["color" .= ("rgba(0,0,0,0.4)" :: Text)]],
          "margin" .= object ["t" .= (40 :: Int), "b" .= (40 :: Int), "l" .= (60 :: Int), "r" .= (20 :: Int)]
        ]
    colors :: [Text] =
      T.words "#4394E5 #87BB62 #876FD4 #F5921B"

factorSmall :: Prim.GeneralFactor -> Text
factorSmall factor = T.concat [T.strip factor.name, "=", T.strip factor.value]

factorsSmall :: Set Prim.GeneralFactor -> Text
factorsSmall = T.intercalate "," . map factorSmall . Set.toList
