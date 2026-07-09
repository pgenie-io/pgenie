-- | Integration with generator adapters.
module GenBridge
  ( Input,
    Output,
    Gen,
    Location (..),
    load,
    bundle,
  )
where

import GenBridge.Bundle
import GenBridge.Load
import GenBridge.Location
import GenBridge.Model
