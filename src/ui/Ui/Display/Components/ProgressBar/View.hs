module Ui.Display.Components.ProgressBar.View
  ( render,
  )
where

import Data.Text qualified as Text
import TextBuilder
import Utils.Prelude

render :: Double -> TextBuilder
render progress =
  let width = 30
      filledWidth = min width (floor (progress * fromIntegral width))
      emptyWidth = width - filledWidth
      filled = to (Text.replicate filledWidth "=")
      arrow = if filledWidth > 0 && filledWidth < width then ">" else ""
      empty = to (Text.replicate (emptyWidth - Text.length arrow) ".")
      percentage =
        let tenths = fromIntegral (floor (progress * 1000)) / 10 :: Double
         in to (Text.pack (show tenths <> "%"))
   in mconcat ["[", filled, to arrow, empty, "] ", percentage]
