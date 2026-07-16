module GenBridge.Load
  ( load,
  )
where

import Data.Text qualified as Text
import Dhall qualified
import Dhall.Core qualified
import Dhall.Import qualified
import Dhall.JSONToDhall qualified
import GenBridge.Contract qualified as Gen
import GenBridge.Dhall.ExprViews qualified as ExprViews
import GenBridge.Dispatch qualified as Dispatch
import GenBridge.Location qualified as Location
import GenContractVersioning qualified
import Lens.Micro qualified
import Utils.Prelude

-- * Procedures

-- | Load a Dhall generator and compute its semantic integrity hash.
--
-- Returns a tuple of (Gen, hash) where the hash is in the format "sha256:..."
-- as produced by @dhall freeze@.
load ::
  Location.Location ->
  -- | Optional integrity hash to verify and cache the loaded generator.
  Maybe Text ->
  -- | Info logging callback to report progress during loading.
  (Text -> IO ()) ->
  -- | Warning logging callback to report non-fatal issues during loading.
  (Text -> IO ()) ->
  IO (Gen.Gen, Text)
load location hash echo warn = do
  let code =
        mconcat [Location.toCode location, maybe "" (\h -> " " <> h) hash]

  echo ("Loading generator code from: " <> to code)

  let settings = Lens.Micro.set Dhall.reportWarning warn Dhall.defaultInputSettings

  genExpr <- Dhall.inputExprWithSettings settings code

  contractVersionExpr <- case ExprViews.recordField "contractVersion" genExpr of
    Nothing -> do
      fail "Could not find 'contractVersion' field in the loaded generator code"
    Just expr -> pure expr

  GenContractVersioning.ContractVersion major minor <- do
    let decoder = Dhall.auto @GenContractVersioning.ContractVersion

    Dhall.expectWithSettings Dhall.defaultInputSettings decoder contractVersionExpr

    Dhall.rawInput decoder contractVersionExpr

  configTypeExpr <- case ExprViews.recordField "Config" genExpr of
    Nothing -> do
      fail "Could not find 'Config' field in the loaded generator code"
    Just expr -> pure expr

  compileExpr <- case ExprViews.recordField "compile" genExpr of
    Nothing -> do
      fail "Could not find 'compile' field in the loaded generator code"
    Just expr -> pure expr

  -- Compute the semantic integrity hash of the loaded expression
  -- According to Dhall spec, hashes must be computed on alpha-beta-normalized expressions
  let normalizedExpr = Dhall.Core.alphaNormalize (Dhall.Core.normalize genExpr)
      hash = Dhall.Import.hashExpressionToCode normalizedExpr

      buildGen :: forall input output. (Dhall.ToDhall input, Dhall.FromDhall output) => Dispatch.Adapters input output -> Either Text Gen.Gen
      buildGen Dispatch.Adapters {projectInput, liftOutput} = Right \config -> do
        configValExpr <- case config of
          Nothing ->
            Right (Dhall.Core.App Dhall.Core.None configTypeExpr)
          Just configJson ->
            case Dhall.JSONToDhall.dhallFromJSON Dhall.JSONToDhall.defaultConversion configTypeExpr configJson of
              Left err ->
                Left ("Config does not conform to the expected schema:\n" <> onto (show err))
              Right configValExpr ->
                Right (Dhall.Core.Some configValExpr)

        let configEncoder =
              Dhall.Encoder
                { embed = const configValExpr,
                  declared = Dhall.Core.App Dhall.Core.Optional configTypeExpr
                }
            decoder =
              fmap
                ($ ())
                ( Dhall.function
                    configEncoder
                    Dhall.auto
                )

        case Dhall.rawInput decoder compileExpr of
          Nothing -> Left "Failed to decode the 'compile' function from the generator code."
          Just (compileFunc :: input -> output) ->
            Right \project ->
              case projectInput project of
                Left err -> Gen.ErrOutput Gen.Report {path = [], message = err}
                Right input -> liftOutput (compileFunc input)

  gen <- case Dispatch.dispatch major minor buildGen of
    Left err -> fail (Text.unpack err)
    Right gen -> pure gen

  pure (gen, hash)
