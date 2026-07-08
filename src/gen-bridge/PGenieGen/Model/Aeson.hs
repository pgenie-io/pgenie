{-# OPTIONS_GHC -Wno-orphans #-}

module PGenieGen.Model.Aeson where

import Data.Aeson qualified as Aeson
import PGenieGen.Model.Input qualified as Input

-- * Input

deriving anyclass instance Aeson.ToJSON Input.Name

deriving anyclass instance Aeson.FromJSON Input.Name

deriving anyclass instance Aeson.ToJSON Input.Version

deriving anyclass instance Aeson.FromJSON Input.Version

deriving anyclass instance Aeson.ToJSON Input.Primitive

deriving anyclass instance Aeson.FromJSON Input.Primitive

deriving anyclass instance Aeson.ToJSON Input.Scalar

deriving anyclass instance Aeson.FromJSON Input.Scalar

deriving anyclass instance Aeson.ToJSON Input.ArraySettings

deriving anyclass instance Aeson.FromJSON Input.ArraySettings

deriving anyclass instance Aeson.ToJSON Input.Value

deriving anyclass instance Aeson.FromJSON Input.Value

deriving anyclass instance Aeson.ToJSON Input.Member

deriving anyclass instance Aeson.FromJSON Input.Member

deriving anyclass instance Aeson.ToJSON Input.EnumVariant

deriving anyclass instance Aeson.FromJSON Input.EnumVariant

deriving anyclass instance Aeson.ToJSON Input.CustomTypeDefinition

deriving anyclass instance Aeson.FromJSON Input.CustomTypeDefinition

deriving anyclass instance Aeson.ToJSON Input.CustomType

deriving anyclass instance Aeson.FromJSON Input.CustomType

deriving anyclass instance Aeson.ToJSON Input.ResultRowsCardinality

deriving anyclass instance Aeson.FromJSON Input.ResultRowsCardinality

deriving anyclass instance Aeson.ToJSON Input.ResultRows

deriving anyclass instance Aeson.FromJSON Input.ResultRows

deriving anyclass instance Aeson.ToJSON Input.Result

deriving anyclass instance Aeson.FromJSON Input.Result

deriving anyclass instance Aeson.ToJSON Input.Var

deriving anyclass instance Aeson.FromJSON Input.Var

deriving anyclass instance Aeson.ToJSON Input.QueryFragment

deriving anyclass instance Aeson.FromJSON Input.QueryFragment

deriving anyclass instance Aeson.ToJSON Input.Query

deriving anyclass instance Aeson.FromJSON Input.Query

deriving anyclass instance Aeson.ToJSON Input.Migration

deriving anyclass instance Aeson.FromJSON Input.Migration

deriving anyclass instance Aeson.ToJSON Input.Project

deriving anyclass instance Aeson.FromJSON Input.Project
