module Infra.Adapters.Display.TextBuilders where

import Base.Prelude
import Data.Text qualified as Text
import Logic qualified
import TextBuilder qualified
import TextBuilderDev qualified

-- | ANSI escape codes for terminal control
clearLine :: TextBuilder
clearLine = "\ESC[2K"

moveCursorToLineStart :: TextBuilder
moveCursorToLineStart = "\r"

-- | Render an ASCII progress bar
progressBar :: Double -> TextBuilder
progressBar progress =
  let width =
        30
      filledWidth =
        round (progress * fromIntegral width)
      emptyWidth =
        width - filledWidth
      filled =
        to (Text.replicate filledWidth "=")
      arrow =
        if filledWidth > 0 && filledWidth < width then ">" else ""
      empty =
        to (Text.replicate (emptyWidth - Text.length arrow) ".")
      percentage =
        TextBuilderDev.doubleFixedPointPercent 1 progress
   in mconcat
        [ "[",
          filled,
          to arrow,
          empty,
          "]  ",
          percentage
        ]
