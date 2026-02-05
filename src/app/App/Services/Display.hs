module App.Services.Display where

import Base.Prelude

newtype Context = Context (MVar Memory)

data Memory = Memory
  { progress :: Double
  }
