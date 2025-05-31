{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module AppAlgebra.CliApp where

import AppAlgebra.Algebra
import Base.Prelude
import GenAlgebra qualified as Gen
import Options.Applicative qualified as Opt

data Command = Command
  { name :: Text,
    description :: Text,
    procedureArgParser :: forall m. (Effect m) => Opt.Parser ([Gen.Gen] -> m ())
  }

-- |
-- Construct an application by specifying the abstract commands and the effect to execute them.
--
-- Parses the arguments.
main ::
  -- | List of supported commands.
  [Command] ->
  -- | Supported generators.
  [Gen.Gen] ->
  -- | Execute an effect.
  (forall m. (Effect m) => m () -> IO ()) ->
  -- | Application.
  IO ()
main _ _ _ =
  error "TODO"
