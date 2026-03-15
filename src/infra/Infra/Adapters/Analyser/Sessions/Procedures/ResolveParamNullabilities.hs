module Infra.Adapters.Analyser.Sessions.Procedures.ResolveParamNullabilities
  ( ResolveParamNullabilities (..),
  )
where

import Data.Vector qualified as Vector
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import HasqlDev qualified as Hasql
import Infra.Adapters.Analyser.Sessions.Algebras.Procedure
import Infra.Adapters.Analyser.Sessions.Domain
import Infra.Adapters.Analyser.Sessions.Procedures.ResolveParamNullabilities.DefaultEncoder qualified as DefaultEncoder
import SyntacticClass qualified as Syntactic
import Utils.Prelude

data ResolveParamNullabilities = ResolveParamNullabilities
  { query :: Text,
    paramTypes :: Vector Type
  }
  deriving stock (Show, Eq)

instance Procedure ResolveParamNullabilities where
  type ProcedureResult ResolveParamNullabilities = Vector Param
  runProcedure params = do
    encoders <- Vector.iforM params.paramTypes \index type_ ->
      inContext
        ["param:", Syntactic.toTextBuilder (show index)]
        case DefaultEncoder.fromType type_ of
          Nothing ->
            crash
              ["Unsupported type"]
              [ ("type", Syntactic.toText (show type_))
              ]
          Just encoder -> pure encoder
    nullabilities <- Vector.fromList <$> Hasql.runSession (go mempty (Vector.toList encoders) [])
    return
      ( Vector.zipWith
          (\type_ nullable -> Param {nullable, type_})
          params.paramTypes
          nullabilities
      )
    where
      go !determinedParamsEncoder !remainingValueEncoders !nullabilities =
        case remainingValueEncoders of
          remainingValueEncodersHead : remainingValueEncodersTail ->
            tryError attempt >>= \case
              Right () -> goWithNullable
              Left (Hasql.Errors.StatementSessionError _ _ _ _ _ (Hasql.Errors.ServerStatementError (Hasql.Errors.ServerError "23502" _ _ _ _))) ->
                goWithNonNullable
              Left err -> throwError err
            where
              goWithNullable =
                go
                  (determinedParamsEncoder <> headNullParamsEncoder)
                  remainingValueEncodersTail
                  (True : nullabilities)
              goWithNonNullable =
                go
                  (determinedParamsEncoder <> nonNullParamsEncoder remainingValueEncodersHead)
                  remainingValueEncodersTail
                  (False : nullabilities)
              headNullParamsEncoder = nullParamsEncoder remainingValueEncodersHead
              attempt =
                Session.statement () statement
                where
                  statement =
                    Statement.unpreparable params.query encoder decoder
                    where
                      encoder =
                        determinedParamsEncoder
                          <> headNullParamsEncoder
                          <> foldMap nonNullParamsEncoder remainingValueEncodersTail
                      decoder =
                        Decoders.noResult
          _ -> return $ reverse nullabilities

nullParamsEncoder :: Encoders.Value () -> Encoders.Params ()
nullParamsEncoder =
  contramap (const Nothing) . Encoders.param . Encoders.nullable

nonNullParamsEncoder :: Encoders.Value () -> Encoders.Params ()
nonNullParamsEncoder =
  Encoders.param . Encoders.nonNullable
