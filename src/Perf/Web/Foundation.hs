{-# options -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Perf.Web.Foundation where

import Data.Text (Text)
import Yesod
import Perf.Web.Types

mkYesodData "App" [parseRoutes|
  / HomeR GET
  /branch/#Text BranchR GET
  /branch/#Text/#Text BranchCommitR GET
  /commit/#Text CommitR GET
  /hooks/update ReceiverR POST
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing

instance YesodBreadcrumbs App where
  breadcrumb r =
    case r of
      HomeR -> return ("Home",Nothing)
      BranchR name -> return (name, Nothing)
      CommitR hash -> return (hash, Nothing)
      BranchCommitR branch commit -> return (commit, Just $ BranchR branch)
      ReceiverR -> return ("Webhook Receiver", Nothing)
