{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Useful utilities for using Lucid with Yesod.

module Yesod.Lucid
  (module Yesod.Lucid
  ,module Yesod
  ,module Lucid
  ,module Control.Monad.Reader
  ,Page(..))
  where

import Data.Functor.Identity
import           Control.Monad.Reader
import           Data.Text (Text)
import           Lucid hiding (for_)
import           Lucid.Base
import           Yesod (ToTypedContent, MonadHandler, ToContent, Route, HandlerSite,
                        TypedContent, HasContentType(..))
import qualified Yesod as Y

-- | Page information that the view renderer typically needs.
data Page y = Page
    { url :: Route y -> Text
    , route :: Maybe (Route y)
    , crumbs :: [(Route y, Text)]
    }

-- | Output some lucid, passes a URL renderer to the continuation.
lucid :: (Y.YesodBreadcrumbs y,
 Show (Route y),
 Eq (Route y)) =>
 HtmlT (Reader (Page y)) () -> Y.HandlerFor y (Html ())
lucid m = do
    render <- Y.getUrlRender
    mroute <- Y.getCurrentRoute
    (title, breadcrumbs) <- Y.breadcrumbs
    let env = Page
         render
         mroute
         (breadcrumbs ++
          [ (route, title)
          | Just route <- [mroute] ])
    return
      $ hoistHtmlT (Identity . flip runReader env)
      $ m

instance ToTypedContent (Html ()) where
  toTypedContent m =
    Y.TypedContent (getContentType (Just m))
    (Y.toContent m)

instance ToContent (Html ()) where
  toContent html =
    Y.ContentBuilder (runIdentity (execHtmlT html))
                     Nothing

instance HasContentType (Html ()) where
  getContentType _ = "text/html"
