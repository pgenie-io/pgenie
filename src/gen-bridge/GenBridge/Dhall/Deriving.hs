{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Standards for DerivingVia.
module GenBridge.Dhall.Deriving
  ( module Dhall.Deriving,
    SumModifier,
    FieldModifier,
  )
where

import Data.Maybe
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Dhall.Deriving
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude

-- | Convert constructors from @<Alternative><Type>@ to Dhall alternatives.
--
-- Example: @OptionalResultRowsCardinality@ -> @Optional@.
data SumModifier (suffix :: Symbol)

instance (KnownSymbol suffix) => ModifyOptions (SumModifier suffix) where
  modifyOptions = addConstructorModifier (textFunction @PascalCase . dropSuffix)
    where
      suffix = Text.pack (symbolVal @suffix Proxy)

      dropSuffix text =
        fromMaybe text (Text.stripSuffix suffix text)

-- | Render Haskell record field names as Dhall's conventional camelCase.
type FieldModifier = Field CamelCase
