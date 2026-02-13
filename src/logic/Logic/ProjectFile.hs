{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module Logic.ProjectFile where

import Base.Prelude hiding (Version)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Logic.Algebra qualified as Algebra
import Logic.Name qualified as Name
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen

data ProjectFile = ProjectFile
  { space :: Name.Name,
    name :: Name.Name,
    version :: Gen.Version,
    artifacts :: [Artifact]
  }

data Artifact = Artifact
  { name :: Name.Name,
    gen :: Gen.Location,
    config :: Aeson.Value
  }

tryFromYaml :: (MonadError Algebra.Error m) => Text -> m ProjectFile
tryFromYaml text =
  error "TODO"
