module PGenieGen.Model.Output
  ( Output (..),
    Report (..),
    Result (..),
    File (..),
  )
where

import Dhall qualified
import Dhall.Deriving qualified
import PGenieGen.Dhall.Deriving qualified as Dhall.Deriving
import PGenieGen.Dhall.Orphans ()
import PGenieGen.Model.Output.Report (Report (..))
import PGenieGen.Prelude

data Output = Output
  { warnings :: [Report],
    result :: Result
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

data Result
  = ResultOk [File]
  | ResultErr Report
  deriving stock (Generic, Show, Eq)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Result") Result)

data File = File
  { path :: Path,
    content :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)
