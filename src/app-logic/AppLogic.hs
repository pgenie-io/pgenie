{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module AppLogic where

import AppAlgebra
import AppAlgebra.Migrations
import Base.Prelude hiding (writeFile)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict qualified as Map
import GenAlgebra qualified as Gen
import ParallelismLogic
import System.FilePath qualified as FilePath

check :: (Effect m) => m ()
check = do
  projectFileLoaded <- loadProjectFile
  analyse projectFileLoaded
  pure ()

generate :: (Effect m) => m ()
generate = do
  projectFileLoaded <- loadProjectFile
  queriesMetadataMerged <- analyse projectFileLoaded
  genProject <- assembleGenProject projectFileLoaded queriesMetadataMerged
  generateCode projectFileLoaded genProject
  pure ()

extractAllMentionedCustomTypes :: (Effect m) => QueriesMetadataMerged -> m (Map Gen.Name Gen.CustomType)
extractAllMentionedCustomTypes = foldM step Map.empty . Map.toList
  where
    step map (queryName, queryIntrospected) =
      foldM step' map (Map.toList queryIntrospected.mentionedCustomTypes)
      where
        step' map (name, customType) =
          case Map.lookup name map of
            Just _ -> error "TODO: handle duplicate custom types"
            Nothing -> do
              pure (Map.insert name customType map)

assembleGenProject :: (Effect m) => ProjectFileLoaded -> QueriesMetadataMerged -> m Gen.Project
assembleGenProject projectFileLoaded queriesMetadataMerged = do
  mentionedCustomTypes <- extractAllMentionedCustomTypes queriesMetadataMerged
  pure
    Gen.Project
      { Gen.name = projectFileLoaded.name,
        Gen.version = projectFileLoaded.version,
        Gen.customTypes = mentionedCustomTypes,
        Gen.queries = Map.map (.query) queriesMetadataMerged
      }

generateCode :: (Effect m) => ProjectFileLoaded -> Gen.Project -> m CodeGenerated
generateCode projectFileLoaded genProject = do
  gensAvail <- ask
  let gensAvailMap =
        gensAvail
          & fmap (\gen -> ((gen.configSectionKey, gen.version), gen))
          & Map.fromList
  artifacts <- forM projectFileLoaded.artifacts \(artifactName, genName, genVersion, genConfigJson) -> do
    Gen.Gen configSectionKey version generatorConfigParser generate <- case Map.lookup (genName, genVersion) gensAvailMap of
      Nothing ->
        throwError (UnknownGenError artifactName genName genVersion)
      Just gen ->
        pure gen
    genConfig <- case Aeson.parse generatorConfigParser genConfigJson of
      Aeson.Error errString ->
        throwError (GenConfigParsingError artifactName genName genVersion (onto errString))
      Aeson.Success genConfigParsed ->
        pure genConfigParsed
    case generate genConfig genProject of
      Left err ->
        throwError (GenError artifactName genName genVersion err)
      Right generatedFiles -> do
        let artifactPath = to @FilePath artifactName
        -- TODO: check if the artifact path exists, create it if not
        overwriting <- pure False
        generatedFilePaths <- for generatedFiles \(path, content) -> do
          let modifiedPath = FilePath.combine artifactPath path
          writeFile modifiedPath content
          pure modifiedPath
        pure (CodeGeneratedArtifact artifactName generatedFilePaths overwriting)
  pure (CodeGenerated artifacts)

analyse :: (Effect m) => ProjectFileLoaded -> m QueriesMetadataMerged
analyse projectFileLoaded = do
  (migrationsListed, queriesListed) <-
    runParallelly do
      (,)
        <$> parallelly (listMigrations projectFileLoaded.migrationsDir)
        <*> parallelly (listQueries projectFileLoaded)

  forM_ migrationsListed \migrationListed -> do
    migrationLoaded <- loadMigration migrationListed
    executeMigration migrationLoaded

  runParallelly do
    Map.fromList
      <$> for queriesListed \queryListed -> parallelly do
        (queryIntrospected, querySignatureLoaded) <- runParallelly do
          (,)
            <$> parallelly do
              querySqlLoaded <- loadQuerySql queryListed
              querySqlParsed <- parseQuerySql querySqlLoaded
              introspectQuery querySqlParsed
            <*> parallelly do
              loadQuerySignature projectFileLoaded queryListed
        merged <- mergeQueryMetadata queryIntrospected querySignatureLoaded
        pure (queryListed.name, merged)
