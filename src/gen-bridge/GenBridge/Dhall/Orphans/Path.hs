{-# OPTIONS_GHC -Wno-orphans #-}

module GenBridge.Dhall.Orphans.Path where

import AlgebraicPath qualified as Path
import Dhall.Marshal.Decode
import Dhall.Marshal.Encode
import GenBridge.Dhall.Decode
import Utils.Prelude

instance ToDhall Path where
  injectWith normalizer =
    Path.toText >$< injectWith normalizer

instance FromDhall Path where
  autoWith normalizer =
    narrow Path.maybeFromText (autoWith normalizer)
