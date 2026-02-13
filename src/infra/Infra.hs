module Infra
  ( run,
  )
where

import Base.Prelude
import Data.Text.IO qualified as Text
import Fx
import Infra.Adapters.Main qualified as MainAdapter
import Logic qualified
import TextBuilder qualified

run :: Fx MainAdapter.Device Logic.Error () -> IO ()
run fx =
  fx
    & scoping MainAdapter.scope
    & handleErr
      ( \err ->
          runTotalIO \_dev -> do
            Text.putStrLn
              $ from @TextBuilder
              $ mconcat
              $ [ "\n\ESC[1;31mError\ESC[0m: ",
                  to err.message,
                  "\n",
                  if null err.path
                    then ""
                    else
                      "Location: "
                        <> TextBuilder.intercalateMap " > " to err.path
                        <> "\n",
                  maybe "" (mappend "Suggestion: " . to . mappend "\n") err.suggestion,
                  if null err.details
                    then ""
                    else
                      "Details:\n"
                        <> TextBuilder.intercalateMap
                          "\n"
                          ( \(key, value) ->
                              "  " <> to key <> ": " <> to value
                          )
                          err.details
                        <> "\n"
                ]
      )
    & Fx.runFx
