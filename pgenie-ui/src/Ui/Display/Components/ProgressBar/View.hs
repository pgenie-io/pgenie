-- |
-- Rendering of the progress-bar's fixed-width ASCII bar and percentage readout.
module Ui.Display.Components.ProgressBar.View
  ( render,
  )
where

import Data.Text qualified as Text
import TextBuilder
import Utils.Prelude

-- | Render a fixed-width (30 column) bar and a one-decimal percentage for the
-- given fraction complete (0 to 1).
render :: Double -> TextBuilder
render progress =
  let width = 30
      filledWidth = min width (floor (progress * fromIntegral @Int width))
      emptyWidth = width - filledWidth
      filled = to (Text.replicate filledWidth "=")
      arrow = if filledWidth > 0 && filledWidth < width then ">" else ""
      empty = to (Text.replicate (emptyWidth - Text.length arrow) ".")
      percentage =
        let tenths = fromIntegral (floor @_ @Int (progress * 1000)) / 10 :: Double
         in to (Text.pack (show tenths <> "%"))
   in mconcat ["[", filled, to arrow, empty, "] ", percentage]
