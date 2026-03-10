module AesonDeriver
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

options :: Text -> Options
options sumSuffix =
  defaultOptions
    { sumEncoding =
        ObjectWithSingleField,
      constructorTagModifier =
        \tag ->
          let text =
                Text.pack tag
              stripped =
                case Text.stripSuffix sumSuffix text of
                  Just stripped -> stripped
                  Nothing -> case Text.stripPrefix sumSuffix text of
                    Just stripped -> stripped
                    Nothing -> error ("Prefix/suffix stripping failed: " <> show (sumSuffix, tag))
              slug =
                Cases.process Cases.lower Cases.spinal stripped
              string =
                Text.unpack slug
           in string,
      fieldLabelModifier =
        Text.unpack . Cases.process Cases.lower Cases.spinal . Text.pack
    }

derive :: [TH.Name] -> TH.Q [TH.Dec]
derive typeNames =
  typeNames
    & traverse (\typeName -> deriveJSON (options (Text.pack (TH.nameBase typeName))) typeName)
    & fmap concat
