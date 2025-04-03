module Perf.Types.Prim where

import Database.Persist.Sqlite
import Data.Text (Text)
import Lucid.Base (ToHtml)
import Data.String
import Yesod (PathPiece)

newtype SubjectName = SubjectName Text
  deriving newtype (Show, Eq, Ord, PersistField, PersistFieldSql, IsString, Read, ToHtml, PathPiece)

newtype Hash = Hash Text
  deriving newtype (Show, Eq, Ord, PersistField, PersistFieldSql, IsString, Read, ToHtml, PathPiece)

newtype MetricLabel = MetricLabel Text
  deriving newtype (Show, Eq, Ord, PersistField, PersistFieldSql, IsString, Read, ToHtml, PathPiece)
