{-# options -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Perf.Web.Dispatch where
import Perf.Web.Routes
import Perf.Web.Types
import Perf.Web.Foundation
import Yesod
mkYesodDispatch "App" resourcesApp
