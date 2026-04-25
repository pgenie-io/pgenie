module Logic.GeneratorHashes
  ( HashesMap,
    Port (..),
    tryLoadHashesFile,
    serializeHashesMap,
    writeHashesFile,
  )
where

import Control.Foldl qualified as Fold
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Logic.Report qualified as Report
import Utils.Prelude hiding (readFile, writeFile)
import YamlUnscrambler qualified as U

-- | Port for accessing the generator hashes file.
class (MonadError Report.Report m) => Port m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()

-- * Types

-- | Map of generator URLs to their integrity hashes.
type HashesMap = Map Text Text

-- * Constants

hashesFilePath :: Path
hashesFilePath = "freeze1.pgn.yaml"

-- * Operations

-- | Try to load the hashes file. Returns an empty map if the file doesn't exist or can't be parsed.
tryLoadHashesFile :: (Port m) => m HashesMap
tryLoadHashesFile =
  catchError
    (parseHashesFile <$> readFile hashesFilePath)
    (\(_ :: Report.Report) -> pure Map.empty)

writeHashesFile :: (Port m) => HashesMap -> m ()
writeHashesFile hashes =
  writeFile hashesFilePath (serializeHashesMap hashes)

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
