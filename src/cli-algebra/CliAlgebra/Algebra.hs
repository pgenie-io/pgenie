module CliAlgebra.Algebra where

import AppAlgebra
import Base.Prelude
import Data.Text qualified as Text
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
  (Effect m) =>
  -- | Name of the application.
  Text ->
  -- | Description of the application.
  Text ->
  -- | List of supported commands.
  [Command] ->
  -- | Execute an effect.
  (m () -> IO ()) ->
  -- | Application.
  IO ()
main appName description commands runEffect =
  join (Opt.execParser parserInfo)
  where
    parserInfo :: Opt.ParserInfo (IO ())
    parserInfo =
      Opt.info
        (Opt.helper <*> Opt.hsubparser (foldMap runCommand commands))
        ( mconcat
            [ Opt.fullDesc,
              Opt.progDesc (Text.unpack description),
              Opt.header (Text.unpack appName)
            ]
        )

    runCommand :: Command -> Opt.Mod Opt.CommandFields (IO ())
    runCommand Command {..} =
      Opt.command
        (Text.unpack name)
        ( Opt.info
            (runEffect . execute <$> parser)
            (Opt.progDesc (Text.unpack description))
        )
