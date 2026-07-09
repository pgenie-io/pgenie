{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Aggregates the 'Dhall.ToDhall'\/'Dhall.FromDhall' orphan instances for
-- types that "GenBridge.Model" needs but doesn't own. Import this module
-- for its instances only.
module GenBridge.Dhall.Orphans where

import GenBridge.Dhall.Orphans.NonEmpty ()
import GenBridge.Dhall.Orphans.Path ()
