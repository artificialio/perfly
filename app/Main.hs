import Perf.Web
import Criterion.Measurement
import System.Posix.Signals
import Control.Concurrent

main :: IO ()
main = do
  me <- myThreadId
  _ <- installHandler sigTERM (Catch (killThread me)) Nothing
  initializeTime
  server
