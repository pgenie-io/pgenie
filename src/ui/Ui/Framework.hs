module Ui.Framework where

import Base.Prelude
import Data.Text qualified as Text
import Options.Applicative qualified as Opt

-- |
-- Command with procedure that may be implemented in terms of abstract algebra,
-- allowing to delegate the execution of effects out to the main function.
data Command m = forall params. Command
  { name :: Text,
    description :: Text,
    parser :: Opt.Parser params,
    execute :: params -> m ()
  }

-- |
-- Construct an application by specifying the abstract commands and the effect to execute them.
--
-- Parses the arguments.
main ::
  -- | Name of the application.
  Text ->
  -- | Description of the application.
  Text ->
  -- | List of supported commands.
  [Command m] ->
  -- | Execute an effect.
  (m () -> IO ()) ->
  -- | Application.
  IO ()
main appName description commands runEffect =
  join (Opt.execParser parserInfo)
  where
    parserInfo =
      Opt.info
        (Opt.helper <*> Opt.hsubparser (foldMap runCommand commands))
        ( mconcat
            [ Opt.fullDesc,
              Opt.progDesc (Text.unpack description),
              Opt.header (Text.unpack appName)
            ]
        )

    runCommand Command {..} =
      Opt.command
        (Text.unpack name)
        ( Opt.info
            (runEffect . execute <$> parser)
            (Opt.progDesc (Text.unpack description))
        )
