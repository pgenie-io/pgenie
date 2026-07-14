{-# LANGUAGE TemplateHaskell #-}

-- |
-- The @Input@ half of gen-contract v5: the project description (custom
-- types, queries, migrations) that pgenie hands to a Dhall generator's
-- @compile@ function. Mirrors the shape of the Dhall @Contract@ and is
-- decoded from/encoded to it via the 'Dhall.FromDhall'/'Dhall.ToDhall'
-- instances, and to/from JSON via the pinned kebab-case contract (see
-- "GenContractBase.Aeson.Deriver").
module GenContractV5.Input
  ( -- * Re-exports (identical across every gen-contract major version so far)
    Name (..),
    Version (..),
    Primitive (..),
    EnumVariant (..),
    Migration (..),
    Var (..),
    QueryFragment (..),
    ResultRowsCardinality (..),

    -- * Project
    Project (..),

    -- * Custom types
    CustomType (..),
    CustomTypeDefinition (..),
    CustomTypeRef (..),

    -- * Queries
    Query (..),

    -- * Results
    Result (..),
    ResultRows (..),

    -- * Fields and values
    Member (..),
    Value (..),
    Scalar (..),
  )
where

import GenContractV5.Input.CustomType (CustomType (..))
import GenContractV5.Input.CustomTypeDefinition (CustomTypeDefinition (..))
import GenContractV5.Input.CustomTypeRef (CustomTypeRef (..))
import GenContractV5.Input.EnumVariant (EnumVariant (..))
import GenContractV5.Input.Member (Member (..))
import GenContractV5.Input.Migration (Migration (..))
import GenContractV5.Input.Name (Name (..))
import GenContractV5.Input.Primitive (Primitive (..))
import GenContractV5.Input.Project (Project (..))
import GenContractV5.Input.Query (Query (..))
import GenContractV5.Input.QueryFragment (QueryFragment (..))
import GenContractV5.Input.Result (Result (..))
import GenContractV5.Input.ResultRows (ResultRows (..))
import GenContractV5.Input.ResultRowsCardinality (ResultRowsCardinality (..))
import GenContractV5.Input.Scalar (Scalar (..))
import GenContractV5.Input.Value (Value (..))
import GenContractV5.Input.Var (Var (..))
import GenContractV5.Input.Version (Version (..))
