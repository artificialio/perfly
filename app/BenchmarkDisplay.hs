import Data.Aeson (eitherDecodeFileStrict')
import Data.Char (isHexDigit)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Text qualified as T
import Data.Text.Lazy.IO qualified as LTIO
import Database.Persist (SelectOpt (Asc), entityVal, selectList)
import Database.Persist.Sqlite (runMigration, runSqlite)
import Lucid (Html, renderText)
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
    listBranches :: Bool,
    jsonFiles :: [FilePath]
  }

data Source
  = JsonFiles (NonEmpty FilePath)
  | Sqlite FilePath Text Int
  | ListBranches FilePath

main :: IO ()
main = do
  cli <- execParser parserInfo
  source <- validateSource cli
  case source of
    ListBranches sqlite -> listBranchesFromSqlite sqlite
    JsonFiles files -> do
      snapshots <- mapM loadSnapshot $ NonEmpty.toList files
      let html =
            staticLayout_ "Benchmarks" $
              generateExternalPlots $
                materializeExternalSnapshots $
                  NonEmpty.fromList snapshots
      writeAndOpen cli.outputPath html
    Sqlite sqlite branch limit -> do
      htmlBody <- loadBenchmarksHtmlFromSqlite sqlite branch limit
      writeAndOpen cli.outputPath $
        staticLayout_ ("Benchmarks: " <> branch) htmlBody

writeAndOpen :: FilePath -> Html () -> IO ()
writeAndOpen outputPath html = do
  absoluteOutput <- makeAbsolute outputPath
  LTIO.writeFile absoluteOutput (renderText html)
  openFile absoluteOutput

validateSource :: Cli -> IO Source
validateSource cli =
  case (cli.listBranches, cli.sqlitePath, NonEmpty.nonEmpty cli.jsonFiles) of
    (True, Just sqlite, Nothing) -> pure $ ListBranches sqlite
    (True, Nothing, _) -> die "--list-branches requires --sqlite PATH."
    (True, Just _, Just _) -> die "Use --list-branches with --sqlite only, not with JSON files."
    (False, Just sqlite, Nothing) -> pure $ Sqlite sqlite cli.branchName cli.maxCommits
    (False, Nothing, Just files) -> pure $ JsonFiles files
    (False, Just _, Just _) -> die "Use either JSON files or --sqlite, not both."
    (False, Nothing, Nothing) -> die "Provide one or more JSON files, or use --sqlite."

listBranchesFromSqlite :: FilePath -> IO ()
listBranchesFromSqlite sqlite = do
  names <- runSqlite (T.pack sqlite) do
    runMigration DB.migrateAll
    branches <- selectList @DB.Branch [] [Asc DB.BranchName]
    pure $ map ((.branchName) . entityVal) branches
  mapM_ TIO.putStrLn names

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

loadBenchmarksHtmlFromSqlite :: FilePath -> Text -> Int -> IO (Html ())
loadBenchmarksHtmlFromSqlite sqlite branch limit =
  runSqlite (T.pack sqlite) do
    runMigration DB.migrateAll
    branchCommitEntities <- loadBranchCommits branch limit
    case NonEmpty.nonEmpty branchCommitEntities of
      Nothing -> pure mempty
      Just branchCommitsNE -> do
        let isMaster = branch == "master"
            branchCommits = map entityVal branchCommitEntities
        branchBenchmarks <- materializeCommits branchCommitsNE
        if isMaster
          then pure $ generateCommitPlotsWith MasterComparisonDisabled branchCommits branchBenchmarks
          else do
            masterCommitEntities <- loadBranchCommits "master" 10
            let masterCtx = MasterComparisonEnabled (map entityVal masterCommitEntities)
            benchmarks <- case NonEmpty.nonEmpty masterCommitEntities of
              Nothing -> pure branchBenchmarks
              Just masterCommitsNE -> do
                masterBenchmarks <- materializeCommits masterCommitsNE
                pure $ mergeMasterIntoBranch masterBenchmarks branchBenchmarks
            pure $ generateCommitPlotsWith masterCtx branchCommits benchmarks

openFile :: FilePath -> IO ()
openFile path =
  case os of
    "darwin" -> runOpen "open"
    "linux" -> runOpen "xdg-open"
    _ -> putStrLn $ "Wrote " <> path
  where
    runOpen openCommand = do
      status <- rawSystem openCommand [path]
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
    <*> switch
      ( long "list-branches"
          <> help "With --sqlite, print available branch names and exit."
      )
    <*> many
      (strArgument
        ( metavar "JSON_FILES..."
            <> help "JSON files, each containing a top-level array of Benchmark."
        ))
