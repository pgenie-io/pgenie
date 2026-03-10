-- | Standards for DerivingVia.
module PGenieGen.Dhall.Deriving
  ( module Dhall.Deriving,
    SumModifier,
    FieldModifier,
  )
where

import Dhall.Deriving

type SumModifier prefix = Constructor (PascalCase <<< DropPrefix prefix)

type FieldModifier = Field CamelCase
