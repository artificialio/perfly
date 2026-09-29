module Perf.Web.Plot where

import Data.Aeson
import Data.Foldable qualified as Foldable
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Ord (comparing)
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
  -- ^ Master keys loaded with the page, in historical order (oldest first).

-- | Selectable values for the master-commits control.
masterCommitOptions :: Bool -> [Int]
masterCommitOptions viewingMaster
  | viewingMaster = [0, 1, 2, 3, 5, 10, 20, 30, 50, 100, 150, 200, 250, 300, 400, 500]
  | otherwise = [0, 1, 2, 3, 5, 10, 20, 30, 50]

-- | How many master commits to load when viewing master.
maxMasterCommits :: Int
maxMasterCommits = maximum (masterCommitOptions True)

-- | How many master commits to load when viewing a non-master branch.
maxMasterCommitsOnBranch :: Int
maxMasterCommitsOnBranch = maximum (masterCommitOptions False)

-- | How many master commits are shown, and loaded with the page, before the
-- user picks another count.
defaultMasterShown :: Bool -> Int
defaultMasterShown viewingMaster
  | viewingMaster = 20
  | otherwise = 3

generateCommitPlots :: BenchmarkSeries DB.Commit DB.Metric -> Html ()
generateCommitPlots benchmarks =
  generateCommitPlotsWith
    MasterComparisonDisabled
    Nothing
    (collectKeys benchmarks)
    benchmarks

-- | The optional URL serves more master commits on demand (see
-- 'masterSeriesJson'). Without it, the page can only show the master commits
-- it was rendered with.
generateCommitPlotsWith ::
  MasterPlotContext DB.Commit ->
  Maybe Text ->
  [DB.Commit] ->
  BenchmarkSeries DB.Commit DB.Metric ->
  Html ()
generateCommitPlotsWith masterCtx masterSeriesUrl branchCommits =
  generatePlotsWith
    masterCtx
    masterSeriesUrl
    branchCommits
    shortCommitLabel
    (.metricMean)
    (.metricStddev)

generateExternalPlots :: BenchmarkSeries Text DisplayMetric -> Html ()
generateExternalPlots benchmarks =
  generatePlotsWith
    MasterComparisonDisabled
    Nothing
    (collectKeys benchmarks)
    id
    (.mean)
    (.stddev)
    benchmarks

-- | Master commits for the master-series endpoint, in the same shape as the
-- master half of the page's plot data.
masterSeriesJson :: [DB.Commit] -> BenchmarkSeries DB.Commit DB.Metric -> Value
masterSeriesJson commits benchmarks =
  object
    [ "masterLabels" .= map shortCommitLabel commits,
      "master" .= seriesJson (.metricMean) (.metricStddev) commits benchmarks
    ]

-- | Identifies one plotted line across the page and the master-series endpoint.
seriesKey :: Prim.SubjectName -> Prim.MetricLabel -> Set Prim.GeneralFactor -> Text
seriesKey subject metricLabel factors =
  T.intercalate "\t" [subjectText subject, metricText metricLabel, factorsSmall factors]

-- | @[[seriesKey, points]]@, where the points follow @keys@ and are
-- @[mean, stddev]@ or @null@ for a commit without that series.
seriesJson ::
  Ord key =>
  (metric -> Double) ->
  (metric -> Double) ->
  [key] ->
  BenchmarkSeries key metric ->
  Value
seriesJson metricMean metricStddev keys benchmarks =
  toJSON
    [ (seriesKey subject metricLabel factors, map point keys)
    | (subject, tests) <- Map.toList benchmarks,
      (factors, metrics) <- Map.toList tests,
      (metricLabel, metricMap) <- Map.toList metrics,
      let point key = fmap (\metric -> [roundForChart (metricMean metric), roundForChart (metricStddev metric)]) (Map.lookup key metricMap),
      any (`Map.member` metricMap) keys
    ]

-- | Six significant digits are plenty for a chart and keep the JSON small.
roundForChart :: Double -> Double
roundForChart x
  | x == 0 || isNaN x || isInfinite x = x
  | digits >= 0 = fromInteger (round (x * 10 ^ digits)) / 10 ^ digits
  | otherwise = fromInteger (round (x / 10 ^ negate digits)) * 10 ^ negate digits
  where
    digits = 5 - floor (logBase 10 (abs x)) :: Int

generatePlotsWith ::
  Ord key =>
  MasterPlotContext key ->
  Maybe Text ->
  [key] ->
  (key -> Text) ->
  (metric -> Double) ->
  (metric -> Double) ->
  BenchmarkSeries key metric ->
  Html ()
generatePlotsWith masterCtx masterSeriesUrl branchKeys renderKey metricMean metricStddev benchmarks = do
  let masterKeys = case masterCtx of
        MasterComparisonDisabled -> []
        MasterComparisonEnabled cs -> cs
      masterEnabled = case masterCtx of
        MasterComparisonDisabled -> False
        MasterComparisonEnabled {} -> True
      -- On master itself there is no trailing branch series.
      viewingMaster = masterEnabled && null branchKeys
      defaultMasterShow
        | masterEnabled = defaultMasterShown viewingMaster
        | otherwise = 0
      plotData =
        object
          [ "masterLabels" .= map renderKey masterKeys,
            "branchLabels" .= map renderKey branchKeys,
            "master" .= seriesJson metricMean metricStddev masterKeys benchmarks,
            "branch" .= seriesJson metricMean metricStddev branchKeys benchmarks,
            "fetchUrl" .= masterSeriesUrl
          ]
  unless (Map.null benchmarks) do
    plotControls_ masterEnabled defaultMasterShow (masterCommitOptions viewingMaster)
    -- The HTML parser ends a script at "</script", even inside a JSON string.
    script_
      [type_ "application/json", id_ "perf-plot-data"]
      (T.replace "</" "<\\/" (encode' plotData))
  Foldable.for_ (zip [0 :: Int ..] (Map.toList benchmarks)) \(benchmarkIdx, (subject, tests)) -> do
    let metrics =
          orderMetrics $
            Set.fromList $
              concatMap Map.keys $
                Map.elems tests
    div_
      [ class_ "benchmark-subject",
        makeAttributes "data-plot-title" (subjectText subject),
        -- Match the visible subject heading only (not metric labels like "time").
        makeAttributes "data-search-text" (T.toLower (subjectText subject))
      ]
      do
        h2_ $ toHtml subject
        div_ [class_ "chart-grid"] do
          Foldable.for_ (zip [0 :: Int ..] metrics) \(metricIdx, metricLabel) -> do
            let chartLines =
                  zip
                    [factors | (factors, allMetrics) <- Map.toList tests, Map.member metricLabel allMetrics]
                    (cycle plotColors)
                traces =
                  [ object
                      [ "key" .= seriesKey subject metricLabel factors,
                        "name" .= factorsSmall factors,
                        "color" .= color
                      ]
                  | (factors, color) <- chartLines
                  ]
                chartId = T.pack (show benchmarkIdx) <> "-" <> T.pack (show metricIdx)
                legendEntries = [(color, factorsSmall factors) | (factors, color) <- chartLines]
            div_ [class_ "chart-cell"] do
              chart_
                ChartOptions
                  { chartId,
                    traces = toJSON traces,
                    layout = plotLayout metricLabel,
                    heightPx = 360
                  }
              factorLegend_ chartId legendEntries
  unless (Map.null benchmarks) do
    style_ masterTickStyles
    -- Script must run after plot containers are in the DOM.
    script_ plotControlsScript

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
      span_ [id_ "plot-status", style_ "color: #6b7280;"] (pure ())

-- | Color the first N x-axis ticks green via CSS (survives Plotly resize redraws).
-- Plotly often inserts extra SVG siblings next to @g.xtick@, so we select the
-- first N ticks with @:nth-child(-n+N of g.xtick)@ rather than @:nth-child@.
masterTickStyles :: Text
masterTickStyles =
  T.unlines
    [ ".benchmark-plot[data-shown-master=\""
        <> nText
        <> "\"] g.xtick:nth-child(-n+"
        <> nText
        <> " of g.xtick) text { fill: #15803d !important; }"
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
      const statusEl = document.getElementById('plot-status');
      const config = ${plotlyConfigJson};
      const dataEl = document.getElementById('perf-plot-data');
      const model = JSON.parse((dataEl && dataEl.textContent) || '{}');
      const branchLabels = model.branchLabels || [];
      const branchSeries = new Map(model.branch || []);
      const fetchUrl = model.fetchUrl || null;
      let masterLabels = model.masterLabels || [];
      let masterSeries = new Map(model.master || []);
      // Fewer master commits than requested means there are no more to fetch.
      let masterExhausted = masterLabels.length < masterShow();
      // Bumped whenever the plotted points change; a chart drawn at an older
      // version is redrawn when it is next visible.
      let dataVersion = 0;
      let fetchToken = 0;
      const charts = new Map();
      const drawQueue = [];
      let drawScheduled = false;
      const traceVisibility = new Map();
      const singleClickTimers = new Map();
      document.querySelectorAll('.benchmark-plot').forEach((el) => {
        charts.set(el.id, {
          el: el,
          traces: JSON.parse(el.getAttribute('data-traces') || '[]'),
          layout: JSON.parse(el.getAttribute('data-layout') || '{}'),
          visible: false,
          queued: false,
          plotted: false,
          drawnVersion: -1,
          drawnYMode: null
        });
      });
      function setStatus(text) {
        if (statusEl) statusEl.textContent = text;
      }
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
      function yMode() {
        return (!startYAtZero || startYAtZero.checked) ? 'tozero' : 'normal';
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
        const state = charts.get(chartEl.id);
        if (!state) return;
        const vis = ensureVisibility(chartEl.id, state.traces.length);
        syncLegendUI(chartEl);
        if (state.plotted) Plotly.restyle(chartEl, {visible: vis.slice()});
      }
      function pointsFor(series, key, start, count) {
        const pts = series.get(key) || [];
        const out = [];
        for (let i = start; i < start + count; i++) out.push(pts[i] || null);
        return out;
      }
      function buildPlot(state) {
        const shown = Math.min(masterShow(), masterLabels.length);
        const start = masterLabels.length - shown;
        const labels = masterLabels.slice(start).concat(branchLabels);
        const vis = ensureVisibility(state.el.id, state.traces.length);
        const data = state.traces.map((trace, i) => {
          const pts = pointsFor(masterSeries, trace.key, start, shown)
            .concat(pointsFor(branchSeries, trace.key, 0, branchLabels.length));
          return {
            x: labels,
            y: pts.map((p) => p ? p[0] : null),
            type: 'scatter',
            mode: 'lines+markers',
            name: trace.name,
            visible: vis[i],
            line: {color: trace.color, width: 2},
            marker: {size: 7, color: trace.color},
            error_y: {type: 'data', array: pts.map((p) => p ? 2 * p[1] : null), visible: true}
          };
        });
        const layout = Object.assign({}, state.layout, {
          xaxis: Object.assign({}, state.layout.xaxis, {categoryarray: labels}),
          yaxis: Object.assign({}, state.layout.yaxis, {rangemode: yMode()})
        });
        return {data: data, layout: layout, shown: shown};
      }
      function draw(state) {
        state.queued = false;
        if (!state.visible) return;
        const mode = yMode();
        if (state.plotted && state.drawnVersion === dataVersion) {
          if (state.drawnYMode !== mode) {
            state.drawnYMode = mode;
            Plotly.relayout(state.el, {'yaxis.rangemode': mode, 'yaxis.autorange': true});
          }
          return;
        }
        const plot = buildPlot(state);
        state.el.setAttribute('data-shown-master', String(plot.shown));
        if (state.plotted) {
          Plotly.react(state.el, plot.data, plot.layout, config);
        } else {
          Plotly.newPlot(state.el, plot.data, plot.layout, config);
        }
        state.plotted = true;
        state.drawnVersion = dataVersion;
        state.drawnYMode = mode;
        syncLegendUI(state.el);
      }
      // Draw queued charts in animation frames with a time budget, so the page
      // stays responsive while several charts come into view at once.
      function scheduleDraw(state) {
        if (state.queued) return;
        state.queued = true;
        drawQueue.push(state);
        if (!drawScheduled) {
          drawScheduled = true;
          requestAnimationFrame(drainDrawQueue);
        }
      }
      function drainDrawQueue() {
        drawScheduled = false;
        const deadline = performance.now() + 30;
        while (drawQueue.length && performance.now() < deadline) draw(drawQueue.shift());
        if (drawQueue.length) {
          drawScheduled = true;
          requestAnimationFrame(drainDrawQueue);
        }
      }
      function refreshVisiblePlots() {
        charts.forEach((state) => {
          if (state.visible) scheduleDraw(state);
        });
      }
      function loadMaster(count) {
        const token = ++fetchToken;
        setStatus('Loading ' + count + ' master commits...');
        fetch(fetchUrl + '?limit=' + count, {headers: {Accept: 'application/json'}})
          .then((response) => {
            if (!response.ok) throw new Error('HTTP ' + response.status);
            return response.json();
          })
          .then((result) => {
            if (token !== fetchToken) return;
            masterLabels = result.masterLabels || [];
            masterSeries = new Map(result.master || []);
            masterExhausted = masterLabels.length < count;
            setStatus('');
            dataVersion++;
            refreshVisiblePlots();
          })
          .catch((err) => {
            if (token === fetchToken) setStatus('Could not load master commits: ' + err.message);
          });
      }
      function onMasterChange() {
        const count = masterShow();
        if (fetchUrl && !masterExhausted && count > masterLabels.length) {
          loadMaster(count);
          return;
        }
        dataVersion++;
        refreshVisiblePlots();
      }
      function isolateTrace(chartEl, onlyIdx) {
        const state = charts.get(chartEl.id);
        if (!state) return;
        const vis = ensureVisibility(chartEl.id, state.traces.length);
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
            const state = charts.get(chartEl.id);
            if (!state) return;
            const vis = ensureVisibility(chartEl.id, state.traces.length);
            vis[traceIdx] = !vis[traceIdx];
            applyVisibility(chartEl);
          }, 280));
        }
      });
      // Charts hidden by the search box never intersect, so they are not drawn.
      const observer = new IntersectionObserver((entries) => {
        entries.forEach((entry) => {
          const state = charts.get(entry.target.id);
          if (!state) return;
          state.visible = entry.isIntersecting;
          if (state.visible) scheduleDraw(state);
        });
      }, {rootMargin: '400px 0px'});
      charts.forEach((state) => observer.observe(state.el));
      if (search) search.addEventListener('input', applySearch);
      if (masterSelect) masterSelect.addEventListener('change', onMasterChange);
      if (startYAtZero) startYAtZero.addEventListener('change', refreshVisiblePlots);
      applySearch();
    })();
  |]

-- | The script fills in @xaxis.categoryarray@ and @yaxis.rangemode@ from the
-- controls.
plotLayout :: Prim.MetricLabel -> Value
plotLayout metricName =
  object
    [ "title"
        .= object
          [ "text" .= metricText metricName,
            "font" .= object ["family" .= ("monospace" :: Text), "size" .= (16 :: Int)]
          ],
      "xaxis"
        .= object
          [ "title" .= ("" :: Text),
            "type" .= ("category" :: Text),
            "categoryorder" .= ("array" :: Text),
            "tickangle" .= (-30 :: Int),
            "automargin" .= True,
            "tickfont" .= object ["family" .= ("monospace" :: Text), "color" .= ("#111111" :: Text)]
          ],
      "yaxis"
        .= object
          [ "title" .= metricText metricName,
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

-- | Metrics whose names start with "time" come first; the rest stay alphabetical.
orderMetrics :: Set Prim.MetricLabel -> [Prim.MetricLabel]
orderMetrics =
  sortBy (comparing isNotTime <> comparing metricText) . Set.toList
  where
    isNotTime label =
      not $ T.isPrefixOf "time" $ T.toLower $ metricText label

shortCommitLabel :: DB.Commit -> Text
shortCommitLabel commit =
  T.take 8 $
    case commit.commitHash of
      Prim.Hash h -> h
