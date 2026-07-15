-- |
-- The @project1.pgn.yaml@ project file: its schema and YAML parsing.
module Logic.Domain.ProjectFile
  ( ProjectFile (..),
    Artifact (..),
    tryFromYaml,
    spec,
  )
where

import AlgebraicPath qualified as Path
import Control.Foldl qualified as Fold
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import GenBridge qualified as Gen
import GenBridge.Contract qualified as Gen.Input
import Logic.Domain.Name qualified as Name
import Logic.Domain.Report qualified as Report
import Test.Hspec
import Utils.Prelude hiding (Version)
import YamlUnscrambler qualified as U

data ProjectFile = ProjectFile
  { space :: Name.Name,
    name :: Name.Name,
    version :: Gen.Input.Version,
    artifacts :: [Artifact],
    postgres :: Maybe Int
  }

data Artifact = Artifact
  { name :: Name.Name,
    gen :: Gen.Location,
    config :: Maybe Aeson.Value
  }

tryFromYaml :: (MonadError Report.Report m) => Text -> m ProjectFile
tryFromYaml text = do
  case U.parseText projectFileValue text of
    Left errMsg ->
      throwError
        Report.Report
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
          artifacts <- U.atByKey "artifacts" artifactsValue <|> pure []
          postgres <-
            optional
              ( U.atByKey
                  "postgres"
                  ( U.scalarsValue
                      [ U.boundedIntegerScalar (U.Signed False) U.DecimalNumeralSystem
                      ]
                  )
              )
          return ProjectFile {space, name, version, artifacts, postgres}

    nameValue :: U.Value Name.Name
    nameValue =
      U.scalarsValue [U.stringScalar nameString]
      where
        nameString =
          U.formattedString "name" $ \txt ->
            case Name.tryFromText txt of
              Left err -> Left err
              Right name -> Right name

    versionValue :: U.Value Gen.Input.Version
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
                return Gen.Input.Version {major, minor, patch}
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
        [ U.nullScalar Aeson.Null,
          U.boolScalar <&> Aeson.Bool,
          U.scientificScalar <&> Aeson.Number,
          U.stringScalar (fmap Aeson.String U.textString)
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
            [ U.nullScalar Aeson.Null,
              U.boolScalar <&> Aeson.Bool,
              U.scientificScalar <&> Aeson.Number,
              U.stringScalar (fmap Aeson.String U.textString)
            ]
            (Just configMapping)
            (Just configSequence)

-- | Test suite for project file YAML parsing.
spec :: Spec
spec = do
  describe "tryFromYaml" do
    it "parses boolean config values as Aeson.Bool, not Aeson.String" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0\n\
            \artifacts:\n\
            \  java:\n\
            \    gen: https://raw.githubusercontent.com/pgenie-io/java.gen/v0.1.2/gen/Gen.dhall\n\
            \    config:\n\
            \      useOptional: true"
          result = tryFromYaml yaml :: Either Report.Report ProjectFile
      case result of
        Left err ->
          expectationFailure ("Parse failed: " <> show err)
        Right pf -> do
          art <- case pf.artifacts of
            [art] -> pure art
            _ -> fail "Expected exactly one artifact"
          art.config
            `shouldBe` Just
              ( Aeson.Object
                  (KeyMap.fromList [(Key.fromText "useOptional", Aeson.Bool True)])
              )

    it "parses a missing artifacts section as an empty list" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0"
          result = tryFromYaml yaml :: Either Report.Report ProjectFile
      case result of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right pf -> do
          length pf.artifacts `shouldBe` 0
          pf.postgres `shouldBe` Nothing

    it "leaves image unset when not specified" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0\n\
            \"
          result = tryFromYaml yaml :: Either Report.Report ProjectFile
      case result of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right pf -> do
          length pf.artifacts `shouldBe` 0
          pf.postgres `shouldBe` Nothing

    it "parses an explicit image setting" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0\n\
            \postgres: 15\n\
            \"
          result = tryFromYaml yaml :: Either Report.Report ProjectFile
      case result of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right pf -> do
          length pf.artifacts `shouldBe` 0
          pf.postgres `shouldBe` Just 15
