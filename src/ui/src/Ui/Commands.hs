-- |
-- Aggregator of all pGenie CLI commands, re-exported as a single list-friendly
-- namespace for "Ui" to assemble into the command-line framework.
module Ui.Commands
  ( analyse,
    generate,
    manageIndexes,
  )
where

import Ui.Commands.Analyse (analyse)
import Ui.Commands.Generate (generate)
import Ui.Commands.ManageIndexes (manageIndexes)
