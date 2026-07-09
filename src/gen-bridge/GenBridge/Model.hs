-- |
-- The two ends of a generator: the 'Input' project description it consumes,
-- and the 'Output' it produces, joined by the 'Gen' function shape every
-- Dhall generator must expose as its @compile@ field.
module GenBridge.Model
  ( Input,
    Output,
    Gen,
    Compile,
  )
where

import Data.Aeson qualified as Aeson
import GenBridge.Model.Input qualified as Input
import GenBridge.Model.Output qualified as Output
import Utils.Prelude

-- | The project description a generator compiles.
type Input = Input.Project

-- | The files (or error) a generator produces.
type Output = Output.Output

-- | The shape every Dhall generator's @compile@ field is decoded into: given
-- an optional, already-schema-checked config value, either report why the
-- config was rejected or hand back the 'Compile' function.
type Gen = Maybe Aeson.Value -> Either Text (Input -> Output)

-- | A generator with its configuration already applied.
type Compile = Input -> Output
