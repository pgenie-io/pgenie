{-# OPTIONS_GHC -Wno-orphans #-}

module PGenieGen.Dhall.Orphans.NonEmpty where

import Dhall.Marshal.Decode
import Dhall.Marshal.Encode
import PGenieGen.Prelude

instance (ToDhall a) => ToDhall (NonEmpty a) where
  injectWith normalizer =
    recordEncoder
      ( adapt
          >$< encodeFieldWith "head" (injectWith normalizer)
            >*< encodeFieldWith "tail" (injectWith normalizer)
      )
    where
      adapt (h :| t) = (h, t)

instance (FromDhall a) => FromDhall (NonEmpty a) where
  autoWith normalizer =
    record
      ( (:|)
          <$> field "head" (autoWith normalizer)
          <*> field "tail" (autoWith normalizer)
      )
