import Perf.Web
import Criterion.Measurement
import System.Posix.Signals
import System.Exit

main :: IO ()
main = do
  _ <- installHandler sigTERM (Catch (exitWith ExitSuccess)) Nothing
  initializeTime
  server
