module Main where

import Logic qualified
import Test.Hspec
import Utils.Prelude

main :: IO ()
main = hspec Logic.spec
