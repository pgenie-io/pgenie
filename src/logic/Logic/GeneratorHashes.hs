module Logic.GeneratorHashes
  ( HashesMap,
    tryLoadHashesFile,
    serializeHashesMap,
  )
where

import Base.Prelude hiding (readFile, writeFile)
import Control.Foldl qualified as Fold
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Logic.Algebra qualified as Algebra
import PGenieGen qualified as Gen
import YamlUnscrambler qualified as U

-- * Types

-- | Map of generator URLs to their integrity hashes.
type HashesMap = Map Text Text

-- * Constants

hashesFilePath :: Path
hashesFilePath = "freeze.pgn1.yaml"

-- * Operations

-- | Try to load the hashes file. Returns an empty map if the file doesn't exist or can't be parsed.
tryLoadHashesFile :: (Algebra.FsOps m) => m HashesMap
tryLoadHashesFile =
  catchError
    (parseHashesFile <$> Algebra.readFile hashesFilePath)
    (\(_ :: Algebra.Error) -> pure Map.empty)

-- | Serialize the hashes map to YAML format.
serializeHashesMap :: HashesMap -> Text
serializeHashesMap hashes =
  let header = "# Map of generator hashes by url\n"
      entries = Map.toList hashes
      serializedEntries =
        entries
          & map (\(url, hash) -> url <> ": " <> hash)
          & Text.intercalate "\n"
   in if null entries
        then header
        else header <> serializedEntries <> "\n"

-- * Internal

parseHashesFile :: Text -> HashesMap
parseHashesFile text =
  case U.parseText hashesMapValue text of
    Left _ -> Map.empty
    Right hashesMap -> hashesMap

hashesMapValue :: U.Value HashesMap
hashesMapValue =
  U.mappingValue $ U.foldMapping (\k v -> (k, v)) (Fold.foldMap pure Map.fromList) keyString valueString
  where
    keyString = U.textString
    valueString = U.scalarsValue [U.stringScalar U.textString]
