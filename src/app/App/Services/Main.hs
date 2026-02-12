{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures -Wno-dodgy-exports #-}

module App.Services.Main
  ( Context,
    Config (..),
    Error (..),
    Event,

    -- * Procedures
    module Procedures,
  )
where

import App.Services.Main.Context as Context
import App.Services.Main.Procedures as Procedures
