module Perf.Types.External where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, Value)
import Data.Text (Text)

-- | A commit pushed to us externally.
data Commit = Commit
  { -- | What branch this was on
    branch :: Text,
    -- | Commit
    commit :: Text,
    -- | Result
    result :: Result
  } deriving (Generic, Show, Eq)
instance FromJSON Commit

-- | A result for a commit
data Result = Result
  { benchmarks :: [Benchmark]
  } deriving (Generic, Show, Eq)
instance FromJSON Result

-- | Represents a complete test configuration and its results
data Benchmark = Benchmark
  { -- | What is being tested
    subject :: Text,
    -- | List of tests
    tests :: [Test]
  } deriving (Generic, Show, Eq)
instance FromJSON Benchmark

-- | Represents a single test
data Test = Test
  { -- | List of factors that affect the test
    factors :: [Factor],
    -- | List of metrics collected
    metrics :: [Metric]
  } deriving (Generic, Show, Eq)
instance FromJSON Test

-- | Represents a single factor in a test
data Factor = Factor
  { -- | What aspect is being varied
    factor :: Text,
    -- | The value of this factor (can represent both text and numbers)
    value :: Text
  } deriving (Generic, Show, Eq)
instance FromJSON Factor

-- | Represents metrics collected during testing
data Metric = Metric
  { -- | Name of the metric (e.g., "time_ms")
    metric :: Text,
    -- | Lower bound of the range
    rangeLower :: Double,
    -- | Upper bound of the range
    rangeUpper :: Double,
    -- | Mean value
    mean :: Double,
    -- | Standard deviation
    stddev :: Double
  } deriving (Generic, Show, Eq)
instance FromJSON Metric
