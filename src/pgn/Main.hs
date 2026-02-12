module Main (main) where

import Base.Prelude
import Infra qualified
import Ui qualified

main :: IO ()
main =
  Ui.main Infra.run
