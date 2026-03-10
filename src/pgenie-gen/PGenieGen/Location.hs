module PGenieGen.Location where

import AlgebraicPath qualified as Path
import PGenieGen.Prelude

-- | Location of a Dhall generator file.
data Location
  = LocationUrl Text
  | LocationPath Path

-- | Convert a Location to Dhall code.
toCode :: Location -> Text
toCode = \case
  LocationUrl url -> url
  LocationPath path -> Path.toText path
