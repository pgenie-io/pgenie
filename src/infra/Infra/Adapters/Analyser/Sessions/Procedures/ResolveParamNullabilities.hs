module Infra.Adapters.Analyser.Sessions.Procedures.ResolveParamNullabilities
  ( ResolveParamNullabilities (..),
  )
where

import Data.Text qualified as Text
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
    goResult <- Hasql.runSession (go mempty (Vector.toList encoders) [])
    nullabilities <- case goResult of
      Right bools -> pure (Vector.fromList bools)
      Left violationMessage ->
        crashWithSuggestion
          ["The INSERT omits a NOT NULL column that has no DEFAULT value"]
          (notNullViolationSuggestion violationMessage)
          []
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
                -- When a not-null violation occurs with the current parameter
                -- set to null, verify it is indeed caused by *this* parameter
                -- being null by retrying with a non-null value.  If that also
                -- triggers a 23502, the violation originates elsewhere — most
                -- likely an INSERT that omits a NOT NULL column that has no
                -- DEFAULT.
                tryError nonNullAttempt >>= \case
                  Right () -> goWithNonNullable
                  Left (Hasql.Errors.StatementSessionError _ _ _ _ _ (Hasql.Errors.ServerStatementError (Hasql.Errors.ServerError "23502" confirmedMessage _ _ _))) ->
                    -- Confirmed: the 23502 is not caused by the current
                    -- parameter but by a missing NOT NULL column in the INSERT.
                    -- Return the server message so the caller can produce a
                    -- helpful suggestion without depending on OS-specific error
                    -- propagation paths.
                    return (Left confirmedMessage)
                  Left confirmedErr -> throwError confirmedErr
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
              nonNullAttempt =
                Session.statement () statement
                where
                  statement =
                    Statement.unpreparable params.query encoder decoder
                    where
                      encoder =
                        determinedParamsEncoder
                          <> nonNullParamsEncoder remainingValueEncodersHead
                          <> foldMap nonNullParamsEncoder remainingValueEncodersTail
                      decoder =
                        Decoders.noResult
          _ -> return (Right (reverse nullabilities))

notNullViolationSuggestion :: Text -> Text
notNullViolationSuggestion message =
  case Text.stripPrefix "null value in column \"" message of
    Just rest ->
      let col = Text.takeWhile (/= '"') rest
       in "Add \"" <> col <> "\" to the INSERT column list, or define a DEFAULT value for the column in the schema"
    Nothing ->
      "Add all NOT NULL columns to the INSERT statement, or define DEFAULT values for them in the schema"

nullParamsEncoder :: Encoders.Value () -> Encoders.Params ()
nullParamsEncoder =
  contramap (const Nothing) . Encoders.param . Encoders.nullable

nonNullParamsEncoder :: Encoders.Value () -> Encoders.Params ()
nonNullParamsEncoder =
  Encoders.param . Encoders.nonNullable
