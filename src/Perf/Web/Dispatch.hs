{-# options -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Perf.Web.Dispatch where
import Perf.Web.Routes
import Perf.Types.Web
import Perf.Web.Foundation
import Yesod
mkYesodDispatch "App" resourcesApp
