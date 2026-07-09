{-# OPTIONS_GHC -Wno-orphans #-}

module PGenieGen.Model.Aeson where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson qualified as Aeson
import PGenieGen.AesonDeriver qualified as AesonDeriver
import PGenieGen.Model.Input qualified as Input

-- * Name

--
-- Hand-written, not run through 'AesonDeriver.derive': its fields keep
-- their literal camelCase Haskell names as JSON keys.

instance Aeson.ToJSON Input.Name where
  toJSON (Input.Name cc pc kc tc skc sc csc ssc) =
    Aeson.object
      [ "inCamelCase" Aeson..= cc,
        "inPascalCase" Aeson..= pc,
        "inKebabCase" Aeson..= kc,
        "inTrainCase" Aeson..= tc,
        "inScreamingKebabCase" Aeson..= skc,
        "inSnakeCase" Aeson..= sc,
        "inCamelSnakeCase" Aeson..= csc,
        "inScreamingSnakeCase" Aeson..= ssc
      ]

instance Aeson.FromJSON Input.Name where
  parseJSON = Aeson.withObject "Name" \obj ->
    Input.Name
      <$> obj
        Aeson..: "inCamelCase"
      <*> obj
        Aeson..: "inPascalCase"
      <*> obj
        Aeson..: "inKebabCase"
      <*> obj
        Aeson..: "inTrainCase"
      <*> obj
        Aeson..: "inScreamingKebabCase"
      <*> obj
        Aeson..: "inSnakeCase"
      <*> obj
        Aeson..: "inCamelSnakeCase"
      <*> obj
        Aeson..: "inScreamingSnakeCase"

-- * Everything else

--
-- Kebab-case field names, ObjectWithSingleField sum encoding.

AesonDeriver.derive
  [ ''Input.Version,
    ''Input.Primitive,
    ''Input.Scalar,
    ''Input.ArraySettings,
    ''Input.Value,
    ''Input.Member,
    ''Input.EnumVariant,
    ''Input.CustomTypeDefinition,
    ''Input.CustomType,
    ''Input.ResultRowsCardinality,
    ''Input.ResultRows,
    ''Input.Result,
    ''Input.Var,
    ''Input.QueryFragment,
    ''Input.Query,
    ''Input.Project,
    ''Input.Migration
  ]
