-- |
-- Location of a Dhall generator, either fetched over the network or read
-- from the local filesystem.
module GenBridge.Location
  ( Location (..),
    toCode,
  )
where

import AlgebraicPath qualified
import Utils.Prelude

-- | Location of a Dhall generator file.
data Location
  = LocationUrl Text
  | LocationPath Path

-- | Convert a Location to Dhall code.
toCode :: Location -> Text
toCode = \case
  LocationUrl url -> url
  LocationPath path -> AlgebraicPath.toText path
