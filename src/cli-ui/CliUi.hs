module CliUi
  ( main,
  )
where

import AppAlgebra qualified
import Base.Prelude
import CliUi.Commands qualified as Commands
import CommandCliUiAlgebra qualified as Algebra

-- |
-- Construct an application by specifying the abstract commands and the effect to execute them.
--
-- Parses the arguments.
main ::
  (AppAlgebra.Effect m) =>
  -- | Name of the application.
  Text ->
  -- | Description of the application.
  Text ->
  -- | Execute an effect.
  (m () -> IO ()) ->
  -- | Application.
  IO ()
main appName description runEffect =
  Algebra.main appName description commands runEffect
  where
    commands =
      [ Commands.generate
      ]
