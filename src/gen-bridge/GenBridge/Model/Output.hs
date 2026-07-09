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

data File = File
  { path :: Text,
    content :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)
