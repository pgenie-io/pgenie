{-# LANGUAGE TemplateHaskell #-}

-- |
-- Queries declared in a v4 contract project.
module GenContractV4.Contract.Query
  ( Query (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Contract.Member (Member)
import GenContractV4.Contract.Name (Name)
import GenContractV4.Contract.QueryFragment (QueryFragment)
import GenContractV4.Contract.Result (Result)
import Utils.Prelude

-- | A query with name, source path, parameters, result, and fragments.
data Query = Query
  { name :: Name,
    srcPath :: Text,
    identity :: Bool,
    idempotent :: Bool,
    params :: [Member],
    result :: Result,
    fragments :: [QueryFragment]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary Query where
  arbitrary = Query <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  shrink Query {name, srcPath, identity, idempotent, params, result, fragments} =
    [ Query name' srcPath' identity' idempotent' params' result' fragments'
    | (name', srcPath', identity', idempotent', params', result', fragments') <-
        shrink (name, srcPath, identity, idempotent, params, result, fragments)
    ]

Aeson.Deriver.derive [''Query]
