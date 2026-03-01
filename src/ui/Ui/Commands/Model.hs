-- | Command that outputs the JSON project model transmitted to code generators.
--
-- Useful for debugging what gets produced during the generate command,
-- and for using the application as a source of analysis data.
module Ui.Commands.Model (model) where

import Base.Prelude
import Logic qualified
import Options.Applicative qualified as Opt
import Ui.Framework

model :: (Logic.Caps m) => Command m
model =
  Command
    { name = "model",
      description = "Output the JSON project model transmitted to code generators",
      parser,
      execute
    }

data Params = Params

parser :: Opt.Parser Params
parser = pure Params

execute :: (Logic.Caps m) => Params -> m ()
execute _params = Logic.model
