-- | Command that validates the project without generating code or writing signature files.
module Ui.Commands.Check (check) where

import Base.Prelude
import Logic qualified
import Options.Applicative qualified as Opt
import Ui.Framework

check :: (Logic.Caps m) => Command m
check =
  Command
    { name = "check",
      description = "Validate the project without generating code or writing signature files",
      parser = pure (),
      execute
    }

execute :: (Logic.Caps m) => () -> m Text
execute () = Logic.check $> ""
