{-# LANGUAGE TemplateHaskell #-}

module GenBridge.Bundle
  ( bundle,
  )
where

import Data.Text qualified as Text
import Dhall qualified
import Dhall.Core qualified
import Dhall.JSONToDhall qualified
import GenBridge.Contract qualified as Gen
import GenBridge.Dhall.ExprViews qualified as ExprViews
import GenBridge.Dispatch qualified as Dispatch
import GenBridge.Location qualified as Location
import GenContractVersioning qualified
import Language.Haskell.TH.Syntax qualified as TH
import Utils.Prelude

-- | Imports Dhall at compile time and constructs a typed Haskell compiler function from it.
bundle :: Location.Location -> Maybe Text -> TH.Code TH.Q Gen.Gen
bundle location hash = TH.Code do
  let code = mconcat [Location.toCode location, maybe "" (\h -> " " <> h) hash]

  (major, minor, configTypeExpr, compileExpr) <- TH.runIO do
    putStrLn ("Loading generator code from: " <> to code)

    genExpr <- Dhall.inputExpr code

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

    case Dispatch.dispatch major minor (\_ -> Right ()) of
      Left err -> fail (Text.unpack err)
      Right () -> pure ()

    pure (major, minor, configTypeExpr, compileExpr)

  TH.examineCode
    [||
    \config -> do
      let buildGen :: forall input output. (Dhall.ToDhall input, Dhall.FromDhall output) => Dispatch.Adapters input output -> Either Text Gen.Gen
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

      case Dispatch.dispatch major minor buildGen of
        Left err -> Left err
        Right gen -> gen config
    ||]
