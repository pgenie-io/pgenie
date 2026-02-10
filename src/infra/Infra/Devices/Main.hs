module Infra.Devices.Main where

import AppLogic qualified
import Base.Prelude
import Data.Text.IO qualified as Text
import Fx
import Infra.Devices.Display qualified as Display
import StagingAlgebra qualified
import TextBuilderDev qualified

data Device = Device
  { display :: Display.Device
  }

data Error
  = DisplayError Display.Error

instance StagingAlgebra.Stages (Fx Device Error) where
  stage name substagesCount =
    subtransform
      (.display)
      (\display device -> device {display})
      DisplayError
      (StagingAlgebra.stage name substagesCount)
