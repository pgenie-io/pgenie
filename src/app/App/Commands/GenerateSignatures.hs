{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module App.Commands.GenerateSignatures where

import App.Frameworks.CommandCliApp
import Base.Prelude
import Data.Text qualified as Text
import Options.Applicative qualified as Opt

data GenerateSignatures

instance ModelsCommand GenerateSignatures where
  modelCommandName _ = "generate-signatures"
  modelCommandDescription _ = "Generate missing type signatures in metadata files"
  modelParser =
    error "TODO"
  modelIO _ = do
    error "TODO"
