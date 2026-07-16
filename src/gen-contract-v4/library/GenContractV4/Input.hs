{-# LANGUAGE TemplateHaskell #-}

-- |
-- The @Input@ half of gen-contract v4 -- frozen forever at the v4.0.1
-- shape (reconstructed from @git diff v4.0.1 v5.0.0@ in the gen-contract
-- repo; v4.0.1 only exported more top-level names than v4.0.0, no field
-- changes). This is the oldest supported rung -- it has no 'GenContractVersioning.HasPreviousVersion'
-- instance. See "GenContractV5.Input" for what changed going into v5.
module GenContractV4.Input
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

    -- * Queries
    Query (..),

    -- * Results
    Result (..),
    ResultRows (..),

    -- * Fields and values
    Member (..),
    Value (..),
    ArraySettings (..),
    Scalar (..),
  )
where

import GenContractV4.Input.ArraySettings (ArraySettings (..))
import GenContractV4.Input.CustomType (CustomType (..))
import GenContractV4.Input.CustomTypeDefinition (CustomTypeDefinition (..))
import GenContractV4.Input.EnumVariant (EnumVariant (..))
import GenContractV4.Input.Member (Member (..))
import GenContractV4.Input.Migration (Migration (..))
import GenContractV4.Input.Name (Name (..))
import GenContractV4.Input.Primitive (Primitive (..))
import GenContractV4.Input.Project (Project (..))
import GenContractV4.Input.Query (Query (..))
import GenContractV4.Input.QueryFragment (QueryFragment (..))
import GenContractV4.Input.Result (Result (..))
import GenContractV4.Input.ResultRows (ResultRows (..))
import GenContractV4.Input.ResultRowsCardinality (ResultRowsCardinality (..))
import GenContractV4.Input.Scalar (Scalar (..))
import GenContractV4.Input.Value (Value (..))
import GenContractV4.Input.Var (Var (..))
import GenContractV4.Input.Version (Version (..))
