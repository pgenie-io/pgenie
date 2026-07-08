module PGenieGen.Bundle where

import Dhall qualified
import Dhall.Core qualified
import Dhall.JSONToDhall qualified as Dhall.FromJson
import Language.Haskell.TH.Syntax qualified as TH
import PGenieGen.ContractVersion qualified as ContractVersion
import PGenieGen.Dhall.ExprViews qualified as ExprViews
import PGenieGen.Location qualified as Location
import PGenieGen.Model
import Utils.Prelude

-- | Imports Dhall at compile time and constructs a typed Haskell compiler function from it.
bundle :: Location.Location -> Maybe Text -> TH.Code TH.Q Gen
bundle location hash = TH.Code do
  let code = mconcat [Location.toCode location, maybe "" (\h -> " " <> h) hash]

  (configTypeExpr, compileExpr) <- TH.runIO do
    putStrLn ("Loading generator code from: " <> to code)

    genExpr <- Dhall.inputExpr code

    contractVersionExpr <- case ExprViews.recordField "contractVersion" genExpr of
      Nothing -> do
        fail "Could not find 'contractVersion' field in the loaded generator code"
      Just expr -> pure expr

    ContractVersion.ContractVersion major minor <- do
      let decoder = Dhall.auto @ContractVersion.ContractVersion

      Dhall.expectWithSettings Dhall.defaultInputSettings decoder contractVersionExpr

      Dhall.rawInput decoder contractVersionExpr

    when (major /= ContractVersion.current.major) do
      fail ("Incompatible contract major version: " <> onto (show major) <> ". Expected " <> onto (show ContractVersion.current.major) <> ".")

    when (minor > ContractVersion.current.minor) do
      fail ("Incompatible contract minor version: " <> onto (show minor) <> ". Expected " <> onto (show ContractVersion.current.minor) <> " or lower.")

    configTypeExpr <- case ExprViews.recordField "Config" genExpr of
      Nothing -> do
        fail "Could not find 'Config' field in the loaded generator code"
      Just expr -> pure expr

    compileExpr <- case ExprViews.recordField "compile" genExpr of
      Nothing -> do
        fail "Could not find 'compile' field in the loaded generator code"
      Just expr -> pure expr

    pure (configTypeExpr, compileExpr)

  TH.examineCode
    [||
    \config -> do
      configValExpr <- case config of
        Nothing ->
          Right (Dhall.Core.App Dhall.Core.None configTypeExpr)
        Just configJson ->
          case Dhall.FromJson.dhallFromJSON Dhall.FromJson.defaultConversion configTypeExpr configJson of
            Left err -> do
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
        Just compileFunc -> Right compileFunc
    ||]
