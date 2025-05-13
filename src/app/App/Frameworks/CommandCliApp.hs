module App.Frameworks.CommandCliApp
  ( -- * Application
    runApp,

    -- * Command
    Command,
    modelCommand,

    -- * Model
    ModelsCommand (..),
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
        (Opt.helper <*> Opt.hsubparser (foldMap coerce commands))
        ( mconcat
            [ Opt.fullDesc,
              Opt.progDesc (Text.unpack description),
              Opt.header (Text.unpack appName)
            ]
        )

-- * Command

newtype Command = Command (Opt.Mod Opt.CommandFields (IO ()))

-- | Construct a command by its model type.
modelCommand :: (ModelsCommand model) => Proxy model -> Command
modelCommand proxy =
  Command
    ( Opt.command
        ( Text.unpack
            (modelCommandName proxy)
        )
        ( Opt.info
            ( modelIO
                <$> flip asProxyTypeOf proxy
                <$> modelParser
            )
            ( Opt.progDesc
                ( Text.unpack
                    (modelCommandDescription proxy)
                )
            )
        )
    )

-- * Model

class ModelsCommand model where
  modelCommandName :: Proxy model -> Text
  modelCommandDescription :: Proxy model -> Text
  modelParser :: Opt.Parser model

  -- | Manage resources and execute a procedure, which outputs via `stdout` and `stderr`.
  modelIO :: model -> IO ()
