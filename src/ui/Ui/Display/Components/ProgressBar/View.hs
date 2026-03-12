module Ui.Display.Components.ProgressBar.View
  ( render,
  )
where

import Base.Prelude
import Data.Text qualified as Text
import TextBuilder
import TextBuilderDev qualified

render :: Double -> TextBuilder
render progress =
  let width = 30
      filledWidth = round (progress * fromIntegral width)
      emptyWidth = width - filledWidth
      filled = to (Text.replicate filledWidth "=")
      arrow = if filledWidth > 0 && filledWidth < width then ">" else ""
      empty = to (Text.replicate (emptyWidth - Text.length arrow) ".")
      percentage = TextBuilderDev.doubleFixedPointPercent 1 progress
   in mconcat ["[", filled, to arrow, empty, "] ", percentage]
