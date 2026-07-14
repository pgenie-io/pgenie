{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'Dhall.ToDhall'\/'Dhall.FromDhall' instances for 'Path', encoded as its
-- textual representation.
module GenContractBase.Dhall.Orphans.Path where

import AlgebraicPath qualified
import Dhall.Marshal.Decode
import Dhall.Marshal.Encode
import GenContractBase.Dhall.Decode qualified as Dhall.Decode
import Utils.Prelude

instance ToDhall Path where
  injectWith normalizer =
    AlgebraicPath.toText >$< injectWith normalizer

instance FromDhall Path where
  autoWith normalizer =
    Dhall.Decode.narrow AlgebraicPath.maybeFromText (autoWith normalizer)
