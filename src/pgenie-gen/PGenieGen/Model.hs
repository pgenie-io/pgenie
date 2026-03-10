module PGenieGen.Model where

import Data.Aeson qualified as Aeson
import PGenieGen.Model.Input qualified as Project
import PGenieGen.Model.Output qualified as Output
import PGenieGen.Prelude

type Input = Project.Project

type Output = Output.Output

type Gen = Maybe Aeson.Value -> Either Text (Input -> Output)

type Compile = Input -> Output
