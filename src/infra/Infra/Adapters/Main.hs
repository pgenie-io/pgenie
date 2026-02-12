{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module Infra.Adapters.Main where

import Base.Prelude
import Data.Text.IO qualified as Text
import Fx
import Infra.Adapters.Display qualified as Display
import Logic qualified
import StagingAlgebra qualified

data Device = Device
  { display :: Display.Device
  }

data Error
  = DisplayError Display.Error

instance StagingAlgebra.Stages (Fx Device Error) where
  stage :: forall a. Text -> Int -> Fx Device Error a -> Fx Device Error a
  stage name substagesCount nestedFx = do
    dev :: Device <- runTotalIO pure
    let subFx :: Fx Display.Device Display.Error (Either Error a) =
          nestedFx
            & mapEnv (const dev)
            & exposeErr
    result <-
      StagingAlgebra.stage name substagesCount subFx
        & mapEnv (.display)
        & mapErr DisplayError
    either throwError pure result
