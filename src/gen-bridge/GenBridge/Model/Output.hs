-- |
-- The @Output@ half of the generator contract: what a Dhall generator's
-- @compile@ function returns after being handed an "GenBridge.Model.Input"
-- project — either the generated 'File's (with any non-fatal 'Report'
-- warnings), or a fatal 'Report' explaining why generation failed.
module GenBridge.Model.Output
  ( Output (..),
    OutputOk (..),
    Report (..),
    File (..),
  )
where

import Dhall qualified
import GenBridge.Dhall.Deriving qualified as Dhall.Deriving
import GenBridge.Dhall.Orphans ()
import GenBridge.Prelude

-- | Diagnostic report with a context path and a message.
data Report = Report
  { path :: [Text],
    message :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

-- | Successful generator output.
data OutputOk = OutputOk
  { warnings :: [Report],
    value :: [File]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

data Output
  = OkOutput OutputOk
  | ErrOutput Report
  deriving stock (Generic, Show, Eq)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Output") Output)

-- | A single generated file, relative to the generation output directory.
data File = File
  { path :: Text,
    content :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)
