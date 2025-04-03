{-# options -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Perf.Web.Foundation where

import Data.Coerce
import Data.Text (Text)
import Yesod
import Perf.Types.Web
import Perf.Types.Prim

mkYesodData "App" [parseRoutes|
  / HomeR GET
  /branch/#Text BranchR GET
  /branch/#Text/#Hash BranchCommitR GET
  /commit/#Hash CommitR GET
  /hooks/update ReceiverR POST
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing

instance YesodBreadcrumbs App where
  breadcrumb r =
    case r of
      HomeR -> return ("Home",Nothing)
      BranchR name -> return (name, Just HomeR)
      CommitR hash -> return (coerce hash, Just HomeR)
      BranchCommitR branch commit -> return (coerce commit, Just $ BranchR branch)
      ReceiverR -> return ("Webhook Receiver", Nothing)
