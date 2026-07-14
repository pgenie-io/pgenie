-- |
-- Shared Aeson TH deriver for gen-contract model data types, pinning them
-- to one JSON contract: kebab-case everywhere, sum types encoded as a
-- single-field object keyed by the constructor's discriminating part (its
-- type-name suffix stripped and spinal-cased).
module GenContractBase.Aeson.Deriver
  ( derive,
  )
where

import Cases qualified
import Data.Aeson.TH
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH
import Prelude

-- | Derive 'Data.Aeson.ToJSON'/'Data.Aeson.FromJSON' for each given type,
-- using the module's pinned kebab-case, single-field-object contract.
derive :: [TH.Name] -> TH.Q [TH.Dec]
derive typeNames =
  typeNames
    & traverse (\typeName -> deriveJSON (options (Text.pack (TH.nameBase typeName))) typeName)
    & fmap concat
  where
    options :: Text -> Options
    options sumSuffix =
      defaultOptions
        { sumEncoding =
            ObjectWithSingleField,
          constructorTagModifier =
            modifyConstructorTag,
          fieldLabelModifier =
            Text.unpack . Cases.process Cases.lower Cases.spinal . Text.pack
        }
      where
        -- Convert constructors from @<Discriminator><TypeName>@ to a
        -- kebab-case tag, e.g. @PrimitiveScalar@ -> @"primitive"@.
        modifyConstructorTag tag =
          Text.unpack slug
          where
            text = Text.pack tag
            stripped =
              case Text.stripSuffix sumSuffix text of
                Just stripped -> stripped
                Nothing -> case Text.stripPrefix sumSuffix text of
                  Just stripped -> stripped
                  Nothing -> error ("Prefix/suffix stripping failed: " <> show (sumSuffix, tag))
            slug = Cases.process Cases.lower Cases.spinal stripped
