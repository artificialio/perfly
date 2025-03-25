import Perf.Web
import Criterion.Measurement
main :: IO ()
main = do
  initializeTime
  server
