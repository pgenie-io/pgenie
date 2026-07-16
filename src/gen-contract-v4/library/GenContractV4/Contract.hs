{-# LANGUAGE TemplateHaskell #-}

-- |
-- The gen-contract v4 data model, mirroring the single flat module that
-- @gen-contract\/src\/package.dhall@ defines -- the Dhall contract has no
-- Input\/Output split, so neither does this.
--
-- ** Input
--
-- Frozen forever at the v4.0.1 shape (reconstructed from @git diff v4.0.1
-- v5.0.0@ in the gen-contract repo; v4.0.1 only exported more top-level
-- names than v4.0.0, no field changes). This is the oldest supported rung --
-- it has no 'GenContractVersioning.HasPreviousVersion' instance. See
-- "GenContractV5.Contract" for what changed going into v5.
--
-- ** Output
--
-- What a Dhall generator's @compile@ function returns after being handed an
-- 'Project' -- either the generated 'File's (with any non-fatal 'Report'
-- warnings), or a fatal 'Report' explaining why generation failed.
-- Shape-identical to "GenContractV5.Contract"'s Output half -- the v5 bump
-- didn't touch Output\/Report\/File -- but kept as its own nominal type per
-- rung, matching the rest of the backcompat algebra.
module GenContractV4.Contract
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

    -- * Output
    Output (..),
    OutputOk (..),
    Report (..),
    File (..),
  )
where

import GenContractV4.Contract.ArraySettings (ArraySettings (..))
import GenContractV4.Contract.CustomType (CustomType (..))
import GenContractV4.Contract.CustomTypeDefinition (CustomTypeDefinition (..))
import GenContractV4.Contract.EnumVariant (EnumVariant (..))
import GenContractV4.Contract.File (File (..))
import GenContractV4.Contract.Member (Member (..))
import GenContractV4.Contract.Migration (Migration (..))
import GenContractV4.Contract.Name (Name (..))
import GenContractV4.Contract.Output (Output (..))
import GenContractV4.Contract.OutputOk (OutputOk (..))
import GenContractV4.Contract.Primitive (Primitive (..))
import GenContractV4.Contract.Project (Project (..))
import GenContractV4.Contract.Query (Query (..))
import GenContractV4.Contract.QueryFragment (QueryFragment (..))
import GenContractV4.Contract.Report (Report (..))
import GenContractV4.Contract.Result (Result (..))
import GenContractV4.Contract.ResultRows (ResultRows (..))
import GenContractV4.Contract.ResultRowsCardinality (ResultRowsCardinality (..))
import GenContractV4.Contract.Scalar (Scalar (..))
import GenContractV4.Contract.Value (Value (..))
import GenContractV4.Contract.Var (Var (..))
import GenContractV4.Contract.Version (Version (..))
