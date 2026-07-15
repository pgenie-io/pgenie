-- |
-- The two ends of a generator: the 'Input' project description it consumes,
-- and the 'Output' it produces, joined by the 'Gen' function shape every
-- Dhall generator must expose as its @compile@ field.
--
-- This module re-exports the full union of "GenContractV5.Input" and
-- "GenContractV5.Output" so that consumers can import every model type from
-- a single location.
module GenBridge.Contract
  ( module GenContractV5.Input,
    module GenContractV5.Output,
    Input,
    Gen,
    Compile,
  )
where

import Data.Aeson qualified as Aeson
import GenContractV5.Input
import GenContractV5.Output
import Utils.Prelude

-- | The project description a generator compiles.
type Input = Project

-- | The shape every Dhall generator's @compile@ field is decoded into: given
-- an optional, already-schema-checked config value, either report why the
-- config was rejected or hand back the 'Compile' function.
type Gen = Maybe Aeson.Value -> Either Text (Input -> Output)

-- | A generator with its configuration already applied.
type Compile = Input -> Output
