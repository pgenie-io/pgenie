{-# LANGUAGE TemplateHaskell #-}

-- |
-- The gen-contract v5 data model, mirroring the single flat module that
-- @gen-contract\/src\/package.dhall@ defines -- the Dhall contract has no
-- Input\/Output split, so neither does this.
--
-- ** Input
--
-- The project description (custom types, queries, migrations) that pgenie
-- hands to a Dhall generator's @compile@ function. Mirrors the shape of the
-- Dhall @Contract@ and is decoded from/encoded to it via the
-- 'Dhall.FromDhall'/'Dhall.ToDhall' instances, and to/from JSON via the
-- pinned kebab-case contract (see "GenContractBase.Aeson.Deriver").
--
-- ** Output
--
-- What a Dhall generator's @compile@ function returns after being handed an
-- 'Input' 'Project' -- either the generated 'File's (with any non-fatal
-- 'Report' warnings), or a fatal 'Report' explaining why generation failed.
-- Shape-identical to "GenContractV4.Contract"'s Output half -- the v5 bump
-- didn't touch Output\/OutputOk\/File\/Report -- so re-exported wholesale
-- rather than redefined as a nominally distinct rung.
module GenContractV5.Contract
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

    -- * Output
    Output (..),
    OutputOk (..),
    Report (..),
    File (..),
  )
where

import GenContractV4.Contract (EnumVariant (..), File (..), Migration (..), Name (..), Output (..), OutputOk (..), Primitive (..), QueryFragment (..), Report (..), ResultRowsCardinality (..), Var (..), Version (..))
import GenContractV5.Contract.CustomType (CustomType (..))
import GenContractV5.Contract.CustomTypeDefinition (CustomTypeDefinition (..))
import GenContractV5.Contract.CustomTypeRef (CustomTypeRef (..))
import GenContractV5.Contract.Member (Member (..))
import GenContractV5.Contract.Project (Project (..))
import GenContractV5.Contract.Query (Query (..))
import GenContractV5.Contract.Result (Result (..))
import GenContractV5.Contract.ResultRows (ResultRows (..))
import GenContractV5.Contract.Scalar (Scalar (..))
import GenContractV5.Contract.Value (Value (..))
