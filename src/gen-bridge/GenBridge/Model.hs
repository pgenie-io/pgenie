module GenBridge.Model where

import Data.Aeson qualified as Aeson
import GenBridge.Model.Input qualified as Project
import GenBridge.Model.Output qualified as Output
import Utils.Prelude

type Input = Project.Project

type Output = Output.Output

type Gen = Maybe Aeson.Value -> Either Text (Input -> Output)

type Compile = Input -> Output
