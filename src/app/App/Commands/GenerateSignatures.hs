{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module App.Commands.GenerateSignatures (generateSignatures) where

import App.Frameworks.CommandCliApp
import Base.Prelude
import Data.Text qualified as Text
import Options.Applicative qualified as Opt

generateSignatures :: Command
generateSignatures =
  Command
    { name = "generate-signatures",
      description = "Generate missing type signatures in metadata files",
      parser,
      execute
    }

data Params = Params
  { force :: Bool
  }

parser :: Opt.Parser Params
parser =
  Params
    <$> Opt.switch
      ( Opt.long "force"
          <> Opt.short 'f'
          <> Opt.help "Force generation of signatures, even if they already exist"
      )

execute :: Params -> IO ()
execute =
  error "TODO"
