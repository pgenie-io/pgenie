{-# OPTIONS_GHC -Wno-orphans #-}

-- | JSON serialization for the project model transmitted to code generators.
module Logic.ProjectModel
  ( toJsonText,
  )
where

import AlgebraicPath qualified as Path
import Base.Prelude
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import PGenieGen.Model.Input qualified as Gen.Input

-- * Orphan ToJSON instances

instance Aeson.ToJSON Path.Path where
  toJSON = Aeson.toJSON . Path.toText

instance Aeson.ToJSON Gen.Input.Version

instance Aeson.ToJSON Gen.Input.WordChar

instance Aeson.ToJSON Gen.Input.WordOrNumber

instance Aeson.ToJSON Gen.Input.Name

instance Aeson.ToJSON Gen.Input.Primitive

instance Aeson.ToJSON Gen.Input.Scalar

instance Aeson.ToJSON Gen.Input.ArraySettings

instance Aeson.ToJSON Gen.Input.Value

instance Aeson.ToJSON Gen.Input.Member

instance Aeson.ToJSON Gen.Input.EnumVariant

instance Aeson.ToJSON Gen.Input.CustomTypeDefinition

instance Aeson.ToJSON Gen.Input.CustomType

instance Aeson.ToJSON Gen.Input.ResultRowsCardinality

instance Aeson.ToJSON Gen.Input.ResultRows

instance Aeson.ToJSON Gen.Input.Var

instance Aeson.ToJSON Gen.Input.QueryFragment

instance Aeson.ToJSON Gen.Input.Query

instance Aeson.ToJSON Gen.Input.Project

-- * Serialization

-- | Serialize the project model to a JSON text.
toJsonText :: Gen.Input.Project -> Text
toJsonText = to . Aeson.encodeToTextBuilder
