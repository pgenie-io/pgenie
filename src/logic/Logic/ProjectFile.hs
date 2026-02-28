module Logic.ProjectFile where

import AlgebraicPath qualified as Path
import Base.Prelude hiding (Version)
import Control.Foldl qualified as Fold
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Logic.Algebra qualified as Algebra
import Logic.Name qualified as Name
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen
import YamlUnscrambler qualified as U

data ProjectFile = ProjectFile
  { space :: Name.Name,
    name :: Name.Name,
    version :: Gen.Version,
    artifacts :: [Artifact]
  }

data Artifact = Artifact
  { name :: Name.Name,
    gen :: Gen.Location,
    config :: Maybe Aeson.Value
  }

tryFromYaml :: (MonadError Algebra.Error m) => Text -> m ProjectFile
tryFromYaml text = do
  case U.parseText projectFileValue text of
    Left errMsg ->
      throwError
        Algebra.Error
          { path = ["project1.pgn.yaml"],
            message = errMsg,
            suggestion = Just "Check YAML syntax and required fields",
            details = []
          }
    Right projectFile -> return projectFile
  where
    projectFileValue :: U.Value ProjectFile
    projectFileValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ do
          space <- U.atByKey "space" nameValue
          name <- U.atByKey "name" nameValue
          version <- U.atByKey "version" versionValue
          artifacts <- U.atByKey "artifacts" artifactsValue
          return ProjectFile {space, name, version, artifacts}

    nameValue :: U.Value Name.Name
    nameValue =
      U.scalarsValue [U.stringScalar nameString]
      where
        nameString =
          U.formattedString "name" $ \txt ->
            case Name.tryFromText txt of
              Left err -> Left err
              Right name -> Right name

    versionValue :: U.Value Gen.Version
    versionValue =
      U.scalarsValue [U.stringScalar versionString]
      where
        versionString =
          U.formattedString "version (major.minor.patch)" $ \txt ->
            case Text.splitOn "." txt of
              [majorText, minorText, patchText] -> do
                major <- parseNatural "major" majorText
                minor <- parseNatural "minor" minorText
                patch <- parseNatural "patch" patchText
                return Gen.Version {major, minor, patch}
              _ ->
                Left "Invalid version format. Use semantic versioning format: major.minor.patch (e.g., 1.0.0)"
        parseNatural fieldName txt =
          case readMaybe (Text.unpack txt) of
            Nothing ->
              Left ("Invalid natural number for " <> fieldName <> ": " <> txt)
            Just n -> return n

    artifactsValue :: U.Value [Artifact]
    artifactsValue =
      U.mappingValue
        $ U.foldMapping merge Fold.list artifactKeyString artifactValue
      where
        -- Set the name field on the artifact
        merge artifactName (gen, config) = Artifact {name = artifactName, gen, config}

        -- Parse and normalize artifact names
        artifactKeyString =
          U.formattedString "artifact-name" $ \txt ->
            let normalized = normalizeArtifactName txt
             in case Name.tryFromText normalized of
                  Left err -> Left ("Invalid artifact name: " <> err)
                  Right name -> Right name
          where
            normalizeArtifactName txt =
              -- Replace hyphens with underscores and prefix purely numeric parts with 'v'
              let parts = Text.splitOn "_" (Text.replace "-" "_" txt)
                  normalizedParts = map normalizePart parts
               in Text.intercalate "_" normalizedParts
              where
                normalizePart part
                  | Text.null part = part
                  | isDigit (Text.head part) = "v" <> part
                  | otherwise = Text.toLower part

        artifactValue =
          U.value
            [U.stringScalar locationString] -- Short form: just a URL string
            (Just artifactMapping) -- Long form: object with gen and optional config
            Nothing
          where
            locationString =
              U.formattedString "location" $ \txt -> do
                gen <- parseLocation txt
                return (gen, Nothing)

            artifactMapping =
              U.byKeyMapping (U.CaseSensitive True) $ do
                genText <- U.atByKey "gen" (U.scalarsValue [U.stringScalar genLocationString])
                config <-
                  asum
                    [ U.atByKey "config" configValue,
                      pure Nothing
                    ]
                return (genText, config)

            genLocationString =
              U.formattedString "location" parseLocation

            parseLocation :: Text -> Either Text Gen.Location
            parseLocation txt
              | "http://" `Text.isPrefixOf` txt || "https://" `Text.isPrefixOf` txt =
                  Right (Gen.LocationUrl txt)
              | otherwise =
                  case Path.maybeFromText txt of
                    Nothing -> Left ("Invalid path: " <> txt)
                    Just path -> Right (Gen.LocationPath path)

    configValue :: U.Value (Maybe Aeson.Value)
    configValue =
      U.nullableValue
        [ U.stringScalar (fmap Aeson.String U.textString),
          U.nullScalar Aeson.Null,
          U.boolScalar <&> Aeson.Bool,
          U.scientificScalar <&> Aeson.Number
        ]
        (Just configMapping)
        (Just configSequence)
      where
        configMapping =
          U.foldMapping
            (\k v -> (Key.fromText k, v))
            (Fold.Fold (\acc (k, v) -> (k, v) : acc) [] (Aeson.Object . KeyMap.fromList . reverse))
            U.textString
            deeperValue

        configSequence =
          U.foldSequence
            (Fold.Fold (\acc v -> v : acc) [] (Aeson.Array . Vector.fromList . reverse))
            deeperValue

        deeperValue =
          U.value
            [ U.stringScalar (fmap Aeson.String U.textString),
              U.nullScalar Aeson.Null,
              U.boolScalar <&> Aeson.Bool,
              U.scientificScalar <&> Aeson.Number
            ]
            (Just configMapping)
            (Just configSequence)
