module Logic.Capabilities
  ( -- * Capabilities
    Logic.Capabilities.Fs.FsOps (..),
    Logic.Capabilities.GeneratorRuntime.LoadsGen (..),
    Logic.Capabilities.IndexCatalog.LoadsIndexes (..),
    Logic.Capabilities.Migrations.ExecutesMigrations (..),
    Logic.Capabilities.QueryAnalysis.InfersQueryTypes (..),
    Logic.Capabilities.Reporting.Warns (..),
    Logic.Capabilities.SeqScanExplain.ExplainsQuery (..),
    Logic.Capabilities.Staging.Stages (..),
  )
where

import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.GeneratorRuntime (LoadsGen (..))
import Logic.Capabilities.IndexCatalog (LoadsIndexes (..))
import Logic.Capabilities.Migrations (ExecutesMigrations (..))
import Logic.Capabilities.QueryAnalysis (InfersQueryTypes (..))
import Logic.Capabilities.Reporting (Warns (..))
import Logic.Capabilities.SeqScanExplain (ExplainsQuery (..))
import Logic.Capabilities.Staging (Stages (..))
