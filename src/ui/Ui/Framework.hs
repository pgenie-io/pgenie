-- |
-- Minimal command-line framework: describes a CLI as a list of abstract
-- 'Command's and defers the execution of their effects to the caller.
module Ui.Framework
  ( Command (..),
    main,
  )
where

import Data.Text qualified as Text
import Logic.Domain.ProjectFile qualified as ProjectFile
import Options.Applicative qualified as Opt
import Utils.Prelude

-- |
-- Command with procedure that may be implemented in terms of abstract algebra,
-- allowing to delegate the execution of effects out to the main function.
data Command m = forall params. Command
  { name :: Text,
    description :: Text,
    parser :: Opt.Parser params,
    execute :: ProjectFile.ProjectFile -> params -> m Text
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
  -- | Footer text shown at the bottom of the help output.
  Text ->
  -- | Version string shown by @--version@.
  Text ->
  -- | List of supported commands.
  [Command m] ->
  -- | Execute an effect.
  -- The first argument is the optional database URL supplied via @--database-url@.
  -- The second is whether @--reuse@ was passed (meaningful only in Docker mode).
  -- The effect receives the parsed project file.
  (Maybe Text -> Bool -> (ProjectFile.ProjectFile -> m Text) -> IO ()) ->
  -- | Application.
  IO ()
main appName description footer version commands runEffect =
  join (Opt.execParser parserInfo)
  where
    parserInfo =
      Opt.info
        ( Opt.helper
            <*> Opt.infoOption (Text.unpack version) (Opt.long "version" <> Opt.short 'V' <> Opt.help "Show version")
            <*> ( (\dbUrl reuse action -> action dbUrl reuse)
                    <$> optional
                      ( Opt.strOption
                          ( Opt.long "database-url"
                              <> Opt.metavar "URL"
                              <> Opt.help "PostgreSQL connection string (libpq key=value or URI). When supplied, pGenie connects to this running server instead of starting a Docker container."
                          )
                      )
                    <*> Opt.switch
                      ( Opt.long "reuse"
                          <> Opt.help "Reuse a Docker container across runs instead of starting a fresh one each time. Only meaningful without --database-url. The container is left running; see the README for how to find and remove it."
                      )
                    <*> Opt.hsubparser (foldMap runCommand commands)
                )
        )
        ( mconcat
            [ Opt.fullDesc,
              Opt.progDesc (Text.unpack description),
              Opt.header (Text.unpack appName),
              Opt.footer (Text.unpack footer)
            ]
        )

    runCommand Command {..} =
      Opt.command
        (Text.unpack name)
        ( Opt.info
            ((\params dbUrl reuse -> runEffect dbUrl reuse (\projectFile -> execute projectFile params)) <$> parser)
            (Opt.progDesc (Text.unpack description))
        )
