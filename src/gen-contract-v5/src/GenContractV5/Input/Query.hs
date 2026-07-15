{-# LANGUAGE TemplateHaskell #-}

-- |
-- Queries declared in a v5 contract project.
module GenContractV5.Input.Query
  ( Query (..),
    toV4Query,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input (Name, QueryFragment)
import GenContractV4.Input qualified as V4
import GenContractV5.Input.Member (Member, toV4Member)
import GenContractV5.Input.Result (Result, toV4Result)
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

-- | Downgrade a v5 query to the v4 shape.
toV4Query :: Query -> V4.Query
toV4Query Query {name, srcPath, identity, idempotent, params, result, fragments} =
  V4.Query
    { name,
      srcPath,
      identity,
      idempotent,
      params = map toV4Member params,
      result = toV4Result result,
      fragments
    }

Aeson.Deriver.derive [''Query]
