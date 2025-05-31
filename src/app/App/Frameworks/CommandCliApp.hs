module App.Frameworks.CommandCliApp
  ( -- * Application
    runApp,

    -- * Command
    Command (..),
  )
where

import Base.Prelude
import Data.Text qualified as Text
import Options.Applicative qualified as Opt

-- | Construct a command line application.
runApp ::
  -- | Name of the application.
  Text ->
  -- | Description of the application.
  Text ->
  -- | Supported commands.
  [Command] ->
  IO ()
runApp appName description commands = Opt.execParser parserInfo >>= id
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

-- * Command

runCommand :: Command -> Opt.Mod Opt.CommandFields (IO ())
runCommand (Command name description parser execute) =
  Opt.command
    (Text.unpack name)
    ( Opt.info
        (execute <$> parser)
        (Opt.progDesc (Text.unpack description))
    )

data Command = forall params. Command
  { name :: Text,
    description :: Text,
    parser :: Opt.Parser params,
    -- | Manage resources and execute a procedure, which outputs via `stdout` and `stderr`.
    execute :: params -> IO ()
  }
