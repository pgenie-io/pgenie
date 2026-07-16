{-# LANGUAGE TemplateHaskell #-}

module Gen.Bundle
  ( bundle,
  )
where

import Data.Text qualified as Text
import Dhall qualified
import Dhall.Core qualified
import Dhall.JSONToDhall qualified
import Dhall.TypeCheck qualified
import Gen.Contract qualified as Gen
import Gen.Dhall.ExprViews qualified as ExprViews
import Gen.Location qualified as Location
import GenContractV5 (V5)
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

    contractVersion@(GenContractVersioning.ContractVersion major minor) <- do
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

    case GenContractVersioning.codecByVersion @V5 contractVersion of
      Left err -> fail (Text.unpack (GenContractVersioning.dispatchErrorToText err))
      Right (_ :: GenContractVersioning.Codec Gen.Input Gen.Output) -> pure ()

    pure (major, minor, configTypeExpr, compileExpr)

  TH.examineCode
    [||
    \config -> do
      case GenContractVersioning.codecByVersion @V5 (GenContractVersioning.ContractVersion major minor) of
        Left err -> Left (GenContractVersioning.dispatchErrorToText err)
        Right GenContractVersioning.Codec {encode = codecEncode, decode = codecDecode} -> do
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
                case codecEncode project of
                  Left err -> Gen.ErrOutput Gen.Report {path = [], message = err}
                  Right inputExpr ->
                    let resultExpr = Dhall.Core.App compileWithConfigExpr inputExpr
                     in case Dhall.TypeCheck.typeOf resultExpr of
                          Left typeError ->
                            Gen.ErrOutput Gen.Report {path = [], message = "The generator's 'compile' rejected the project:\n" <> onto (show typeError)}
                          Right _ ->
                            case codecDecode (Dhall.Core.normalize resultExpr) of
                              Left err -> Gen.ErrOutput Gen.Report {path = [], message = err}
                              Right output -> output
    ||]
