import Data.Aeson (eitherDecodeFileStrict')
import Data.Char (isHexDigit)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.IO qualified as LTIO
import Database.Persist
import Database.Persist.Sqlite (runMigration, runSqlite)
import Lucid (renderText)
import Options.Applicative
import Perf.DB.Materialize
import Perf.Types.DB qualified as DB
import Perf.Types.External qualified as EX
import Perf.Web.Layout
import Perf.Web.Plot
import System.Directory (makeAbsolute)
import System.Exit (ExitCode (ExitSuccess), die)
import System.FilePath (takeBaseName)
import System.Info (os)
import System.Process (rawSystem)

data Cli = Cli
  { outputPath :: FilePath,
    sqlitePath :: Maybe FilePath,
    branchName :: Text,
    maxCommits :: Int,
    jsonFiles :: [FilePath]
  }

data Source
  = JsonFiles (NonEmpty FilePath)
  | Sqlite FilePath Text Int

main :: IO ()
main = do
  cli <- execParser parserInfo
  source <- validateSource cli
  html <- case source of
    JsonFiles files -> do
      snapshots <- mapM loadSnapshot $ NonEmpty.toList files
      pure $
        staticLayout_ "Benchmarks" $
          generateExternalPlots $
            materializeExternalSnapshots $
              NonEmpty.fromList snapshots
    Sqlite sqlite branch limit -> do
      benchmarks <- loadBenchmarksFromSqlite sqlite branch limit
      pure $
        staticLayout_ ("Benchmarks: " <> branch) $
          generateCommitPlots benchmarks
  absoluteOutput <- makeAbsolute cli.outputPath
  LTIO.writeFile absoluteOutput (renderText html)
  openFile absoluteOutput

validateSource :: Cli -> IO Source
validateSource cli =
  case (cli.sqlitePath, NonEmpty.nonEmpty cli.jsonFiles) of
    (Just sqlite, Nothing) -> pure $ Sqlite sqlite cli.branchName cli.maxCommits
    (Nothing, Just files) -> pure $ JsonFiles files
    (Just _, Just _) -> die "Use either JSON files or --sqlite, not both."
    (Nothing, Nothing) -> die "Provide one or more JSON files, or use --sqlite."

loadSnapshot :: FilePath -> IO (Text, [EX.Benchmark])
loadSnapshot path = do
  decoded <- eitherDecodeFileStrict' path
  case decoded of
    Left err -> die $ "Failed to decode " <> path <> ": " <> err
    Right benchmarks -> pure (labelFromPath path, benchmarks)

labelFromPath :: FilePath -> Text
labelFromPath path =
  let base = takeBaseName path
      suffix = reverse $ takeWhile (/= '-') $ reverse base
      hasDash = '-' `elem` base
   in if hasDash && not (null suffix) && all isHexDigit suffix
        then T.pack suffix
        else T.pack base

loadBenchmarksFromSqlite :: FilePath -> Text -> Int -> IO (BenchmarkSeries DB.Commit DB.Metric)
loadBenchmarksFromSqlite sqlite branch limit =
  runSqlite (T.pack sqlite) do
    runMigration DB.migrateAll
    mbranch <- selectFirst [DB.BranchName ==. branch] []
    case mbranch of
      Nothing -> pure mempty
      Just (Entity branchId _) -> do
        mappings <-
          selectList
            [DB.MapBranchCommitBranchId ==. branchId]
            [Desc DB.MapBranchCommitId, LimitTo limit]
        commits <- mapM (\mapping -> selectFirst [DB.CommitId ==. mapping.entityVal.mapBranchCommitCommitId] []) mappings
        case NonEmpty.nonEmpty $ reverse $ catMaybes commits of
          Nothing -> pure mempty
          Just existingCommits -> materializeCommits existingCommits

openFile :: FilePath -> IO ()
openFile path =
  case os of
    "darwin" -> runOpen "open"
    "linux" -> runOpen "xdg-open"
    _ -> putStrLn $ "Wrote " <> path
  where
    runOpen command = do
      status <- rawSystem command [path]
      case status of
        ExitSuccess -> pure ()
        _ -> putStrLn $ "Wrote " <> path

parserInfo :: ParserInfo Cli
parserInfo =
  info (cliParser <**> helper) $
    fullDesc
      <> progDesc "Render benchmark graphs into a static HTML file."

cliParser :: Parser Cli
cliParser =
  Cli
    <$> strOption
      ( long "output"
          <> short 'o'
          <> metavar "PATH"
          <> value "benchmark-display.html"
          <> showDefault
          <> help "Output HTML path."
      )
    <*> optional
      (strOption
        ( long "sqlite"
            <> metavar "PATH"
            <> help "Read benchmark data from sqlite database."
        ))
    <*> ( T.pack
            <$> strOption
              ( long "branch"
                  <> metavar "BRANCH"
                  <> value "master"
                  <> showDefault
                  <> help "Branch name to load in sqlite mode."
              )
        )
    <*> option
      auto
      ( long "limit"
          <> metavar "INT"
          <> value 28
          <> showDefault
          <> help "Number of most recent commits in sqlite mode."
      )
    <*> many
      (strArgument
        ( metavar "JSON_FILES..."
            <> help "JSON files, each containing a top-level array of Benchmark."
        ))
