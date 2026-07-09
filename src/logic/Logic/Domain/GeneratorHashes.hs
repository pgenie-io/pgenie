-- |
-- The freeze file that pins generator URLs to their integrity hashes, so
-- repeated runs can detect when a generator has changed.
module Logic.Domain.GeneratorHashes
  ( HashesMap,
    hashesFilePath,
    serializeHashesMap,
    parseHashesFile,
  )
where

import Control.Foldl qualified as Fold
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Utils.Prelude
import YamlUnscrambler qualified as U

-- * Types

-- | Map of generator URLs to their integrity hashes.
type HashesMap = Map Text Text

-- * Constants

-- | Project-relative path of the freeze file.
hashesFilePath :: Path
hashesFilePath = "freeze1.pgn.yaml"

-- * Operations

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

-- | Parse a freeze file. Malformed content is treated as an empty map,
-- since a missing or corrupt freeze file only means no hashes are pinned yet.
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
