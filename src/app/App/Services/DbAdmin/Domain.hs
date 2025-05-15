{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module App.Services.DbAdmin.Domain where

import Base.Prelude

newtype TempDbHandle = TempDbHandle
  { userAndDbName :: Text
  }
  deriving stock (Show, Eq)
