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
              $ [ "Error at location: ",
                  TextBuilder.intercalateMap " > " to err.path,
                  "\nMessage: ",
                  to err.message,
                  maybe "" (mappend "\nSuggestion: " . to) err.suggestion,
                  if null err.details
                    then ""
                    else
                      "\nDetails:\n"
                        <> TextBuilder.intercalateMap
                          "\n"
                          ( \(key, value) ->
                              "  " <> to key <> ": " <> to value
                          )
                          err.details
                ]
      )
    & Fx.runFx
