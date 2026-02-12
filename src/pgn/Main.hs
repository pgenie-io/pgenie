module Main (main) where

import Base.Prelude
import Infra.Adapters.Main qualified
import Ui qualified

main :: IO ()
main =
  Ui.main Infra.Adapters.Main.run
