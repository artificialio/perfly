module Measure where

import Control.Monad.IO.Class
import Criterion.Measurement

measure :: MonadIO m => m a -> m (Double, a)
measure m = do
  x <- liftIO getTime
  a <- m
  y <- liftIO getTime
  pure (y - x, a)
