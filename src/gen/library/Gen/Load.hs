module Gen.Load
  ( load,
  )
where

import Data.Text qualified as Text
import Dhall qualified
import Dhall.Core qualified
import Dhall.Import qualified
import Dhall.JSONToDhall qualified
import Dhall.TypeCheck qualified
import Gen.Contract qualified as Gen
import Gen.Dhall.ExprViews qualified as ExprViews
import Gen.Location qualified as Location
import GenContractV5 (V5)
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

  contractVersion@(GenContractVersioning.ContractVersion major minor) <- do
    let decoder = Dhall.auto @GenContractVersioning.ContractVersion

    Dhall.expectWithSettings Dhall.defaultInputSettings decoder contractVersionExpr

    Dhall.rawInput decoder contractVersionExpr

  codec <- case GenContractVersioning.codecByVersion @V5 contractVersion of
    Left err -> fail (Text.unpack (GenContractVersioning.dispatchErrorToText err))
    Right codec -> pure codec

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

      gen :: Gen.Gen
      gen config = do
        configValExpr <- case config of
          Nothing ->
            Right (Dhall.Core.App Dhall.Core.None configTypeExpr)
          Just configJson ->
            case Dhall.JSONToDhall.dhallFromJSON Dhall.JSONToDhall.defaultConversion configTypeExpr configJson of
              Left err ->
                Left ("Config does not conform to the expected schema:\n" <> onto (show err))
              Right configValExpr ->
                Right (Dhall.Core.Some configValExpr)

        let compileWithConfigExpr = Dhall.Core.App compileExpr configValExpr

        case Dhall.TypeCheck.typeOf compileWithConfigExpr of
          Left typeError ->
            Left ("The generator's 'compile' does not accept the given config:\n" <> onto (show typeError))
          Right _ ->
            Right \project ->
              case codec.encode project of
                Left err -> Gen.ErrOutput Gen.Report {path = [], message = err}
                Right inputExpr ->
                  let resultExpr = Dhall.Core.App compileWithConfigExpr inputExpr
                   in case Dhall.TypeCheck.typeOf resultExpr of
                        Left typeError ->
                          Gen.ErrOutput Gen.Report {path = [], message = "The generator's 'compile' rejected the project:\n" <> onto (show typeError)}
                        Right _ ->
                          case codec.decode (Dhall.Core.normalize resultExpr) of
                            Left err -> Gen.ErrOutput Gen.Report {path = [], message = err}
                            Right output -> output

  pure (gen, hash)
