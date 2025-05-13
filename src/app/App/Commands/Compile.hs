{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module App.Commands.Compile where

import App.Frameworks.CommandCliApp
import Base.Prelude
import Data.Text qualified as Text
import Options.Applicative qualified as Opt

data Compile

instance ModelsCommand Compile where
  modelCommandName _ = "compile"
  modelCommandDescription _ = "Compile the project"
  modelParser =
    error "TODO"
  modelIO _ = do
    error "TODO"
