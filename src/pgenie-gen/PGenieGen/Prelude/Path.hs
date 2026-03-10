{-# OPTIONS_GHC -Wno-orphans #-}

module PGenieGen.Prelude.Path where

import AlgebraicPath
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Prelude

instance Aeson.ToJSON Path where
  toJSON = Aeson.String . toText

instance Aeson.FromJSON Path where
  parseJSON = \case
    Aeson.String text ->
      case maybeFromText text of
        Just path -> pure path
        Nothing -> fail ("Invalid path: " <> show text)
    invalid ->
      Aeson.typeMismatch "String" invalid
