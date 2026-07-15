{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Aggregates the 'Dhall.ToDhall'\/'Dhall.FromDhall' orphan instances that
-- gen-contract model types need but don't own. Import this module for its
-- instances only.
module GenContractBase.Dhall.Orphans where

import GenContractBase.Dhall.Orphans.NonEmpty ()
import GenContractBase.Dhall.Orphans.Path ()
