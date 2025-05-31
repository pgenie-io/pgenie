{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module CliAlgebra.Algebra where

import AppAlgebra
import Base.Prelude
import Options.Applicative qualified as Opt

data Command = forall params. Command
  { name :: Text,
    description :: Text,
    parser :: Opt.Parser params,
    execute :: forall m. (Effect m) => params -> m ()
  }

-- |
-- Construct an application by specifying the abstract commands and the effect to execute them.
--
-- Parses the arguments.
main ::
  -- | List of supported commands.
  [Command] ->
  -- | Execute an effect.
  (forall m. (Effect m) => m () -> IO ()) ->
  -- | Application.
  IO ()
main _ _ =
  error "TODO"
