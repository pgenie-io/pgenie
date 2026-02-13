module Logic.ProjectFile where

import AlgebraicPath qualified as Path
import Base.Prelude hiding (Version)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Yaml qualified as Yaml
import Logic.Algebra qualified as Algebra
import Logic.Name qualified as Name
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen

data ProjectFile = ProjectFile
  { space :: Name.Name,
    name :: Name.Name,
    version :: Gen.Version,
    artifacts :: [Artifact]
  }

data Artifact = Artifact
  { name :: Name.Name,
    gen :: Gen.Location,
    config :: Aeson.Value
  }

tryFromYaml :: (MonadError Algebra.Error m) => Text -> m ProjectFile
tryFromYaml text = do
  -- Parse YAML to Aeson Value
  yamlValue <- case Yaml.decodeEither' (Text.Encoding.encodeUtf8 text) of
    Left parseException ->
      throwError
        Algebra.Error
          { path = ["project.pgn1.yaml"],
            message = "Failed to parse YAML: " <> Text.pack (show parseException),
            suggestion = Just "Check YAML syntax",
            details = []
          }
    Right value -> return value

  -- Parse the top-level object
  case yamlValue of
    Aeson.Object obj -> do
      -- Parse space
      spaceText <- extractField obj "space" >>= extractString "space"
      space <- parseName "space" spaceText

      -- Parse name
      nameText <- extractField obj "name" >>= extractString "name"
      name <- parseName "name" nameText

      -- Parse version
      versionText <- extractField obj "version" >>= extractString "version"
      version <- parseVersion versionText

      -- Parse artifacts
      artifactsValue <- extractField obj "artifacts"
      artifacts <- parseArtifacts artifactsValue

      return ProjectFile {space, name, version, artifacts}
    _ ->
      throwError
        Algebra.Error
          { path = ["project.pgn1.yaml"],
            message = "Expected object at top level",
            suggestion = Nothing,
            details = []
          }
  where
    extractField obj fieldName =
      case Aeson.KeyMap.lookup (Aeson.Key.fromText fieldName) obj of
        Nothing ->
          throwError
            Algebra.Error
              { path = ["project.pgn1.yaml", fieldName],
                message = "Missing required field: " <> fieldName,
                suggestion = Nothing,
                details = []
              }
        Just value -> return value

    extractString _ (Aeson.String txt) = return txt
    extractString fieldName _ =
      throwError
        Algebra.Error
          { path = ["project.pgn1.yaml", fieldName],
            message = "Expected string for field: " <> fieldName,
            suggestion = Nothing,
            details = []
          }

    parseName fieldName txt =
      case Name.tryFromText txt of
        Left err ->
          throwError
            Algebra.Error
              { path = ["project.pgn1.yaml", fieldName],
                message = "Invalid name: " <> err,
                suggestion = Just "Use lowercase letters, digits, and hyphens only",
                details = []
              }
        Right name -> return name

    parseVersion txt =
      case Text.splitOn "." txt of
        [majorText, minorText, patchText] -> do
          major <- parseNatural "version.major" majorText
          minor <- parseNatural "version.minor" minorText
          patch <- parseNatural "version.patch" patchText
          return Gen.Version {major, minor, patch}
        _ ->
          throwError
            Algebra.Error
              { path = ["project.pgn1.yaml", "version"],
                message = "Invalid version format: " <> txt,
                suggestion = Just "Use semantic versioning format: major.minor.patch (e.g., 1.0.0)",
                details = []
              }

    parseNatural fieldName txt =
      case readMaybe (Text.unpack txt) of
        Nothing ->
          throwError
            Algebra.Error
              { path = ["project.pgn1.yaml", fieldName],
                message = "Invalid natural number: " <> txt,
                suggestion = Just "Use a non-negative integer",
                details = []
              }
        Just n -> return n

    normalizeArtifactName txt =
      -- Replace hyphens with underscores and prefix purely numeric parts with 'v'
      let parts = Text.splitOn "_" (Text.replace "-" "_" txt)
          normalizedParts = map normalizePart parts
       in Text.intercalate "_" normalizedParts
      where
        normalizePart part
          | Text.null part = part
          | isDigit (Text.head part) = "v" <> part -- Prefix numeric parts with 'v'
          | otherwise = Text.toLower part

    parseArtifacts (Aeson.Object obj) = do
      forM (Aeson.KeyMap.toList obj) $ \(key, value) -> do
        -- Normalize artifact names: convert to lowercase and ensure parts start with letters
        let keyText = normalizeArtifactName (Aeson.Key.toText key)
        artifactName <- parseName "artifacts" keyText
        parseArtifact artifactName value
    parseArtifacts _ =
      throwError
        Algebra.Error
          { path = ["project.pgn1.yaml", "artifacts"],
            message = "Expected object for artifacts",
            suggestion = Nothing,
            details = []
          }

    parseArtifact artifactName value =
      case value of
        -- Short form: just a URL string
        Aeson.String url -> do
          gen <- parseLocation url
          return Artifact {name = artifactName, gen, config = Aeson.Null}
        -- Long form: object with gen and optional config
        Aeson.Object obj -> do
          genValue <- extractField obj "gen"
          genText <- extractString "gen" genValue
          gen <- parseLocation genText
          config <- case Aeson.KeyMap.lookup (Aeson.Key.fromText "config") obj of
            Nothing -> return Aeson.Null
            Just configValue -> return configValue
          return Artifact {name = artifactName, gen, config}
        _ ->
          throwError
            Algebra.Error
              { path = ["project.pgn1.yaml", "artifacts", Name.toText artifactName],
                message = "Expected string or object for artifact",
                suggestion = Just "Use either a URL string or an object with 'gen' field",
                details = []
              }

    parseLocation txt
      | "http://" `Text.isPrefixOf` txt || "https://" `Text.isPrefixOf` txt =
          return (Gen.LocationUrl txt)
      | otherwise =
          case Path.maybeFromText txt of
            Nothing ->
              throwError
                Algebra.Error
                  { path = ["project.pgn1.yaml"],
                    message = "Invalid path: " <> txt,
                    suggestion = Nothing,
                    details = []
                  }
            Just path -> return (Gen.LocationPath path)
