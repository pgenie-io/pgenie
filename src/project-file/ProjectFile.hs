{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module ProjectFile where

import Base.Prelude
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson

-- | Project file format interface.
data ProjectFile = ProjectFile
  { projectName :: Text,
    generators :: Map Text Aeson.Value
  }
