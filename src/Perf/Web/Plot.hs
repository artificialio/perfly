module Perf.Web.Plot where

import Data.Aeson
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Control.Monad
import Lucid
import Lucid.Base (makeAttributes)
import NeatInterpolation (trimming)
import Perf.DB.Materialize
import Perf.Types.DB qualified as DB
import Perf.Types.Prim qualified as Prim
import Perf.Web.Chart

-- | Whether the "Show master branch commits" control is available.
data MasterPlotContext key
  = MasterComparisonDisabled
  | MasterComparisonEnabled [key]
  -- ^ Master keys in historical order (oldest first), at most 'maxMasterCommits'.

-- | Selectable values for the master-commits control.
masterCommitOptions :: Bool -> [Int]
masterCommitOptions viewingMaster
  | viewingMaster = [0, 1, 2, 3, 5, 10, 20, 30, 50, 100]
  | otherwise = [0, 1, 2, 3, 5, 10, 20]

-- | How many master commits to load when viewing master.
maxMasterCommits :: Int
maxMasterCommits = maximum (masterCommitOptions True)

-- | How many master commits to load when viewing a non-master branch.
maxMasterCommitsOnBranch :: Int
maxMasterCommitsOnBranch = maximum (masterCommitOptions False)

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
    shortCommitLabel
    (.metricMean)
    (.metricStddev)

generateExternalPlots :: BenchmarkSeries Text DisplayMetric -> Html ()
generateExternalPlots benchmarks =
  generatePlotsWith
    MasterComparisonDisabled
    (collectKeys benchmarks)
    id
    (.mean)
    (.stddev)
    benchmarks

generatePlotsWith ::
  Ord key =>
  MasterPlotContext key ->
  [key] ->
  (key -> Text) ->
  (metric -> Double) ->
  (metric -> Double) ->
  BenchmarkSeries key metric ->
  Html ()
generatePlotsWith masterCtx branchKeys renderKey metricMean metricStddev benchmarks = do
  let masterKeys = case masterCtx of
        MasterComparisonDisabled -> []
        MasterComparisonEnabled cs -> cs
      masterCount = length masterKeys
      orderedKeys = masterKeys <> branchKeys
      masterEnabled = case masterCtx of
        MasterComparisonDisabled -> False
        MasterComparisonEnabled {} -> True
      -- On master itself there is no trailing branch series.
      viewingMaster = masterEnabled && null branchKeys
      defaultMasterShow
        | viewingMaster = 20
        | masterEnabled = 1
        | otherwise = 0
  unless (Map.null benchmarks) $
    plotControls_ masterEnabled defaultMasterShow (masterCommitOptions viewingMaster)
  Foldable.for_ (zip [0 :: Int ..] (Map.toList benchmarks)) \(benchmarkIdx, (subject, tests)) -> do
    let metrics :: Set Prim.MetricLabel =
          Set.fromList $ concatMap Map.keys $ Map.elems tests
    div_
      [ class_ "benchmark-subject",
        makeAttributes "data-plot-title" (subjectText subject),
        -- Match the visible subject heading only (not metric labels like "time").
        makeAttributes "data-search-text" (T.toLower (subjectText subject))
      ]
      do
        h2_ $ toHtml subject
        div_ [class_ "chart-grid"] do
          Foldable.for_ (zip [0 :: Int ..] (Set.toList metrics)) \(metricIdx, metricLabel) -> do
            let dataSets =
                  flip map (Map.toList tests) \(factors, allMetrics) ->
                    let metricSeries = Map.findWithDefault Map.empty metricLabel allMetrics
                     in ( factors,
                          toSeries metricMean orderedKeys metricSeries,
                          toSeries ((2 *) . metricStddev) orderedKeys metricSeries
                        )
                labels = map renderKey orderedKeys
                (plotData, layout) = makePlotlyConfig metricLabel labels dataSets
                chartId = T.pack (show benchmarkIdx) <> "-" <> T.pack (show metricIdx)
                legendEntries =
                  zip
                    (cycle plotColors)
                    (map (\(factors, _, _) -> factorsSmall factors) dataSets)
            div_ [class_ "chart-cell"] do
              chart_
                ChartOptions
                  { chartId,
                    plotData,
                    layout,
                    masterCount,
                    heightPx = 360
                  }
              factorLegend_ chartId legendEntries
  unless (Map.null benchmarks) do
    style_ masterTickStyles
    -- Script must run after plot containers are in the DOM.
    script_ plotControlsScript
  where
    toSeries accessor keys metricMap =
      flip map keys \key ->
        maybe Null (toJSON . accessor) $
          Map.lookup key metricMap

collectKeys :: Ord key => BenchmarkSeries key metric -> [key]
collectKeys benchmarks =
  Set.toList $
    Set.fromList $
      concatMap (concatMap Map.keys . Map.elems) $
        concatMap Map.elems $
          Map.elems benchmarks

plotControls_ :: Bool -> Int -> [Int] -> Html ()
plotControls_ masterEnabled defaultMasterShow options = do
  div_
    [ class_ "plot-controls",
      style_ "display: flex; flex-wrap: wrap; gap: 1rem; align-items: center; margin: 1rem 0;"
    ]
    do
      input_
        [ type_ "search",
          id_ "plot-search",
          placeholder_ "Search in plot title",
          style_ "flex: 1 1 30%; min-width: 200px; max-width: 30%; padding: 0.35rem 0.5rem; font-family: monospace;"
        ]
      label_
        [ for_ "master-commits",
          style_ "display: flex; align-items: center; gap: 0.4rem; white-space: nowrap; color: #15803d;"
        ]
        do
          "Show master branch commits:"
          select_
            ( [ id_ "master-commits",
                style_ "font-family: monospace; padding: 0.25rem; color: #111111;"
              ]
                <> [makeAttributes "disabled" "disabled" | not masterEnabled]
            )
            do
              forM_ options \n ->
                option_
                  ( [value_ (T.pack (show n))]
                      <> [makeAttributes "selected" "selected" | n == defaultMasterShow]
                  )
                  (toHtml (show n))
      label_
        [ for_ "start-y-at-zero",
          style_ "display: flex; align-items: center; gap: 0.4rem; white-space: nowrap;"
        ]
        do
          input_
            [ type_ "checkbox",
              id_ "start-y-at-zero",
              makeAttributes "checked" "checked"
            ]
          "Start Y axis at zero"

-- | Color the first N x-axis ticks green via CSS (survives Plotly resize redraws).
masterTickStyles :: Text
masterTickStyles =
  T.unlines
    [ ".benchmark-plot[data-shown-master=\""
        <> nText
        <> "\"] g.xtick:nth-child(-n+"
        <> nText
        <> ") text { fill: #15803d !important; }"
    | n <- masterCommitOptions True
    , n > 0
    , let nText = T.pack (show n)
    ]

plotControlsScript :: Text
plotControlsScript =
  [trimming|
    (function () {
      const search = document.getElementById('plot-search');
      const masterSelect = document.getElementById('master-commits');
      const startYAtZero = document.getElementById('start-y-at-zero');
      const config = ${plotlyConfigJson};
      const traceVisibility = new Map();
      const singleClickTimers = new Map();
      function applySearch() {
        if (!search) return;
        const q = (search.value || '').trim().toLowerCase();
        document.querySelectorAll('.benchmark-subject').forEach((el) => {
          const hay = el.getAttribute('data-search-text') || '';
          el.style.display = (!q || hay.includes(q)) ? '' : 'none';
        });
      }
      function masterShow() {
        if (!masterSelect || masterSelect.disabled) return 0;
        const n = parseInt(masterSelect.value, 10);
        return Number.isFinite(n) ? n : 0;
      }
      function slicePlotData(fullData, masterCount, showMaster) {
        const start = Math.max(0, masterCount - showMaster);
        return fullData.map((trace) => Object.assign({}, trace, {
          x: (trace.x || []).slice(start),
          y: (trace.y || []).slice(start)
        }));
      }
      function ensureVisibility(chartId, traceCount) {
        let vis = traceVisibility.get(chartId);
        if (!vis || vis.length !== traceCount) {
          vis = Array.from({length: traceCount}, () => true);
          traceVisibility.set(chartId, vis);
        }
        return vis;
      }
      function syncLegendUI(chartEl) {
        const vis = traceVisibility.get(chartEl.id) || [];
        const root = chartEl.parentElement;
        if (!root) return;
        root.querySelectorAll('.factor-toggle').forEach((btn, i) => {
          btn.classList.toggle('off', !vis[i]);
        });
      }
      function applyVisibility(chartEl) {
        const count = (chartEl.data && chartEl.data.length) || 0;
        if (!count) return Promise.resolve();
        const vis = ensureVisibility(chartEl.id, count);
        syncLegendUI(chartEl);
        return Plotly.restyle(chartEl, {visible: vis.slice()});
      }
      function redrawPlots() {
        const show = masterShow();
        document.querySelectorAll('.benchmark-plot').forEach((el) => {
          const fullData = JSON.parse(el.getAttribute('data-full') || '[]');
          const layout = JSON.parse(el.getAttribute('data-layout') || '{}');
          layout.yaxis = layout.yaxis || {};
          layout.yaxis.rangemode = (!startYAtZero || startYAtZero.checked) ? 'tozero' : 'normal';
          const masterCount = parseInt(el.getAttribute('data-master-count') || '0', 10) || 0;
          const data = slicePlotData(fullData, masterCount, show);
          ensureVisibility(el.id, data.length);
          const shownMaster = Math.min(show, masterCount);
          el.setAttribute('data-shown-master', String(shownMaster));
          const finish = () => applyVisibility(el);
          const plotted = el.getAttribute('data-plotted') === '1'
            ? Plotly.react(el, data, layout, config)
            : Plotly.newPlot(el, data, layout, config).then(() => el.setAttribute('data-plotted', '1'));
          Promise.resolve(plotted).then(finish);
        });
      }
      function isolateTrace(chartEl, onlyIdx) {
        const count = (chartEl.data && chartEl.data.length) || 0;
        const vis = ensureVisibility(chartEl.id, count);
        for (let i = 0; i < vis.length; i++) vis[i] = i === onlyIdx;
        applyVisibility(chartEl);
      }
      document.addEventListener('click', (e) => {
        const btn = e.target.closest('.factor-toggle');
        if (!btn) return;
        const chartId = btn.getAttribute('data-chart-id');
        const traceIdx = parseInt(btn.getAttribute('data-trace-idx'), 10);
        const chartEl = chartId ? document.getElementById(chartId) : null;
        if (!chartEl || !Number.isFinite(traceIdx)) return;
        if (e.detail === 2) {
          const prev = singleClickTimers.get(btn);
          if (prev) clearTimeout(prev);
          singleClickTimers.delete(btn);
          isolateTrace(chartEl, traceIdx);
          return;
        }
        if (e.detail === 1) {
          const prev = singleClickTimers.get(btn);
          if (prev) clearTimeout(prev);
          singleClickTimers.set(btn, setTimeout(() => {
            singleClickTimers.delete(btn);
            const count = (chartEl.data && chartEl.data.length) || 0;
            const vis = ensureVisibility(chartEl.id, count);
            vis[traceIdx] = !vis[traceIdx];
            applyVisibility(chartEl);
          }, 280));
        }
      });
      if (search) search.addEventListener('input', applySearch);
      if (masterSelect) masterSelect.addEventListener('change', redrawPlots);
      if (startYAtZero) startYAtZero.addEventListener('change', redrawPlots);
      applySearch();
      redrawPlots();
    })();
  |]

makePlotlyConfig ::
  Prim.MetricLabel ->
  [Text] ->
  [(Set Prim.GeneralFactor, [Value], [Value])] ->
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
            "marker" .= object ["size" .= (7 :: Int), "color" .= color],
            "error_y"
              .= object
                [ "type" .= ("data" :: Text),
                  "array" .= errors,
                  "visible" .= True
                ]
          ]
        | ((factors, series, errors), color) <- zip dataSets $ cycle plotColors
      ]
    layout =
      object
        [         "title"
            .= object
              [ "text" .= metricText metricName,
                "font" .= object ["family" .= ("monospace" :: Text), "size" .= (16 :: Int)]
              ],
          "xaxis"
            .= object
              [ "title" .= ("" :: Text),
                "type" .= ("category" :: Text),
                "categoryorder" .= ("array" :: Text),
                "categoryarray" .= labels,
                "tickangle" .= (-30 :: Int),
                "automargin" .= True,
                "tickfont" .= object ["family" .= ("monospace" :: Text), "color" .= ("#111111" :: Text)]
              ],
          "yaxis"
            .= object
              [ "title" .= metricText metricName,
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

factorLegend_ :: Text -> [(Text, Text)] -> Html ()
factorLegend_ chartId entries =
  div_ [class_ "factor-lines"] do
    Foldable.for_ (zip [0 :: Int ..] entries) \(idx, (color, name)) ->
      div_ [class_ "factor-line"] do
        button_
          [ type_ "button",
            class_ "factor-toggle",
            title_ "Click to show or hide this line. Double-click to show only this line.",
            makeAttributes "data-chart-id" chartId,
            makeAttributes "data-trace-idx" (T.pack (show idx))
          ]
          do
            span_ [class_ "factor-swatch", style_ ("background-color: " <> color)] (pure ())
            span_ (toHtml name)

factorSmall :: Prim.GeneralFactor -> Text
factorSmall factor = T.concat [T.strip factor.name, "=", T.strip factor.value]

factorsSmall :: Set Prim.GeneralFactor -> Text
factorsSmall = T.intercalate "," . map factorSmall . Set.toList

subjectText :: Prim.SubjectName -> Text
subjectText (Prim.SubjectName t) = t

metricText :: Prim.MetricLabel -> Text
metricText (Prim.MetricLabel t) = t

shortCommitLabel :: DB.Commit -> Text
shortCommitLabel commit =
  T.take 8 $
    case commit.commitHash of
      Prim.Hash h -> h
