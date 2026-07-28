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
import Lucid.Base (makeAttributes)
import Perf.DB.Materialize
import Perf.Types.DB qualified as DB
import Perf.Types.Prim qualified as Prim
import Perf.Web.Chart

-- | Whether the "Show master branch commits" control is available.
data MasterPlotContext key
  = MasterComparisonDisabled
  | MasterComparisonEnabled [key]
  -- ^ Master keys in historical order (oldest first), at most 10.

generateCommitPlots :: BenchmarkSeries DB.Commit DB.Metric -> Html ()
generateCommitPlots benchmarks =
  generateCommitPlotsWith
    MasterComparisonDisabled
    (collectKeys benchmarks)
    benchmarks

generateCommitPlotsWith ::
  MasterPlotContext DB.Commit ->
  [DB.Commit] ->
  BenchmarkSeries DB.Commit DB.Metric ->
  Html ()
generateCommitPlotsWith masterCtx branchCommits =
  generatePlotsWith
    masterCtx
    branchCommits
    (T.take 8 . (coerce :: Prim.Hash -> Text) . (.commitHash))
    (.metricMean)

generateExternalPlots :: BenchmarkSeries Text DisplayMetric -> Html ()
generateExternalPlots benchmarks =
  generatePlotsWith
    MasterComparisonDisabled
    (collectKeys benchmarks)
    id
    (.mean)
    benchmarks

generatePlotsWith ::
  Ord key =>
  MasterPlotContext key ->
  [key] ->
  (key -> Text) ->
  (metric -> Double) ->
  BenchmarkSeries key metric ->
  Html ()
generatePlotsWith masterCtx branchKeys renderKey metricMean benchmarks = do
  let masterKeys = case masterCtx of
        MasterComparisonDisabled -> []
        MasterComparisonEnabled cs -> cs
      masterCount = length masterKeys
      orderedKeys = masterKeys <> branchKeys
      masterEnabled = case masterCtx of
        MasterComparisonDisabled -> False
        MasterComparisonEnabled {} -> True
  unless (Map.null benchmarks) $ plotControls_ masterEnabled
  Foldable.for_ (zip [0 :: Int ..] (Map.toList benchmarks)) \(benchmarkIdx, (subject, tests)) -> do
    let metrics :: Set Prim.MetricLabel =
          Set.fromList $ concatMap Map.keys $ Map.elems tests
        searchText =
          T.toLower $
            T.unwords $
              coerce subject : map coerce (Set.toList metrics)
    div_
      [ class_ "benchmark-subject",
        makeAttributes "data-plot-title" (coerce subject),
        makeAttributes "data-search-text" searchText
      ]
      do
        h2_ $ toHtml subject
        div_ [class_ "chart-grid"] do
          Foldable.for_ (zip [0 :: Int ..] (Set.toList metrics)) \(metricIdx, metricLabel) -> do
            let dataSets =
                  flip map (Map.toList tests) \(factors, allMetrics) ->
                    (factors, toSeries orderedKeys (Map.findWithDefault Map.empty metricLabel allMetrics))
                labels = map renderKey orderedKeys
                (plotData, layout) = makePlotlyConfig metricLabel labels dataSets
                chartId = T.pack (show benchmarkIdx) <> "-" <> T.pack (show metricIdx)
                legendEntries =
                  zip (cycle plotColors) (map (factorsSmall . fst) dataSets)
            div_ [class_ "chart-cell"] do
              chart_
                ChartOptions
                  { chartId,
                    plotData,
                    layout,
                    masterCount,
                    heightPx = 360
                  }
              factorLegend_ legendEntries
  -- Script must run after plot containers are in the DOM.
  unless (Map.null benchmarks) $ script_ plotControlsScript
  where
    toSeries keys metricMap =
      flip map keys \key ->
        maybe Null (toJSON . metricMean) $
          Map.lookup key metricMap

collectKeys :: Ord key => BenchmarkSeries key metric -> [key]
collectKeys benchmarks =
  Set.toList $
    Set.fromList $
      concatMap (concatMap Map.keys . Map.elems) $
        concatMap Map.elems $
          Map.elems benchmarks

plotControls_ :: Bool -> Html ()
plotControls_ masterEnabled = do
  div_
    [ class_ "plot-controls",
      style_ "display: flex; flex-wrap: wrap; gap: 1rem; align-items: center; margin: 1rem 0;"
    ]
    do
      input_
        [ type_ "search",
          id_ "plot-search",
          placeholder_ "Search in plot title",
          style_ "flex: 1; min-width: 16rem; padding: 0.35rem 0.5rem; font-family: monospace;"
        ]
      label_
        [ for_ "master-commits",
          style_ "display: flex; align-items: center; gap: 0.4rem; white-space: nowrap;"
        ]
        do
          "Show master branch commits:"
          select_
            ( [ id_ "master-commits",
                style_ "font-family: monospace; padding: 0.25rem;"
              ]
                <> [makeAttributes "disabled" "disabled" | not masterEnabled]
            )
            do
              forM_ ([0, 1, 2, 3, 5, 10] :: [Int]) \n ->
                option_
                  ( [value_ (T.pack (show n))]
                      <> [makeAttributes "selected" "selected" | n == 0]
                  )
                  (toHtml (show n))

plotControlsScript :: Text
plotControlsScript =
  T.unlines
    [ "(function () {",
      "  const search = document.getElementById('plot-search');",
      "  const masterSelect = document.getElementById('master-commits');",
      "  const config = {responsive: true, modeBarButtonsToRemove: ['select2d', 'lasso2d']};",
      "  function applySearch() {",
      "    if (!search) return;",
      "    const q = (search.value || '').trim().toLowerCase();",
      "    document.querySelectorAll('.benchmark-subject').forEach((el) => {",
      "      const hay = el.getAttribute('data-search-text') || '';",
      "      el.style.display = (!q || hay.includes(q)) ? '' : 'none';",
      "    });",
      "  }",
      "  function masterShow() {",
      "    if (!masterSelect || masterSelect.disabled) return 0;",
      "    const n = parseInt(masterSelect.value, 10);",
      "    return Number.isFinite(n) ? n : 0;",
      "  }",
      "  function slicePlotData(fullData, masterCount, showMaster) {",
      "    const start = Math.max(0, masterCount - showMaster);",
      "    return fullData.map((trace) => Object.assign({}, trace, {",
      "      x: (trace.x || []).slice(start),",
      "      y: (trace.y || []).slice(start)",
      "    }));",
      "  }",
      "  function redrawPlots() {",
      "    const show = masterShow();",
      "    document.querySelectorAll('.benchmark-plot').forEach((el) => {",
      "      const fullData = JSON.parse(el.getAttribute('data-full') || '[]');",
      "      const layout = JSON.parse(el.getAttribute('data-layout') || '{}');",
      "      const masterCount = parseInt(el.getAttribute('data-master-count') || '0', 10) || 0;",
      "      const data = slicePlotData(fullData, masterCount, show);",
      "      if (el.getAttribute('data-plotted') === '1') {",
      "        Plotly.react(el, data, layout, config);",
      "      } else {",
      "        Plotly.newPlot(el, data, layout, config);",
      "        el.setAttribute('data-plotted', '1');",
      "      }",
      "    });",
      "  }",
      "  if (search) search.addEventListener('input', applySearch);",
      "  if (masterSelect) masterSelect.addEventListener('change', redrawPlots);",
      "  applySearch();",
      "  redrawPlots();",
      "})();"
    ]

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
            "line" .= object ["color" .= color, "width" .= (2 :: Int)],
            "marker" .= object ["size" .= (7 :: Int), "color" .= color]
          ]
        | ((factors, series), color) <- zip dataSets $ cycle plotColors
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
                "tickangle" .= (-30 :: Int),
                "automargin" .= True,
                "tickfont" .= object ["family" .= ("monospace" :: Text)]
              ],
          "yaxis"
            .= object
              [ "title" .= coerce @_ @Text metricName,
                "rangemode" .= ("tozero" :: Text),
                "automargin" .= True,
                "tickfont" .= object ["family" .= ("monospace" :: Text)]
              ],
          "font" .= object ["family" .= ("monospace" :: Text)],
          "hovermode" .= ("x unified" :: Text),
          "showlegend" .= False,
          "margin" .= object ["t" .= (40 :: Int), "b" .= (56 :: Int), "l" .= (64 :: Int), "r" .= (40 :: Int)]
        ]

plotColors :: [Text]
plotColors = T.words "#4394E5 #87BB62 #876FD4 #F5921B #1f77b4 #ff7f0e #2ca02c #d62728"

factorLegend_ :: [(Text, Text)] -> Html ()
factorLegend_ entries =
  div_ [class_ "factor-lines"] do
    Foldable.for_ entries \(color, name) ->
      div_ [class_ "factor-line"] do
        span_ [class_ "factor-swatch", style_ ("background-color: " <> color)] (pure ())
        span_ (toHtml name)

factorSmall :: Prim.GeneralFactor -> Text
factorSmall factor = T.concat [T.strip factor.name, "=", T.strip factor.value]

factorsSmall :: Set Prim.GeneralFactor -> Text
factorsSmall = T.intercalate "," . map factorSmall . Set.toList
