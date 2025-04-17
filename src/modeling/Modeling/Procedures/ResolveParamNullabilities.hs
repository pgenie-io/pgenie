module Modeling.Procedures.ResolveParamNullabilities where

import Base.Prelude
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import HasqlDev qualified as Hasql
import Modeling.Domain
import Modeling.Frameworks.Procedure
import Modeling.Procedures.ResolveParamNullabilities.DefaultEncoder qualified as DefaultEncoder
import SyntacticClass qualified as Syntactic

data Params = Params
  { query :: Text,
    params :: [Param]
  }
  deriving stock (Show, Eq)

-- | Updated parameters.
type Result = [Param]

lifted ::
  ( Hasql.RunsSession m,
    MonadReader Location m,
    MonadError Error m,
    MonadWriter [Error] m
  ) =>
  Params ->
  m Result
lifted params = do
  encoders <- forM params.params \param ->
    inContext
      ["param:", Syntactic.toTextBuilder param.name]
      case DefaultEncoder.fromType param.type_ of
        Nothing -> crash ["Unsupported type: ", Syntactic.toTextBuilder (show param.type_)]
        Just encoder -> pure encoder
  nullabilities <- Hasql.runSession (go mempty encoders [])
  return
    ( zipWith
        (\Param {..} nullability -> Param {nullable = nullability, ..})
        params.params
        nullabilities
    )
  where
    queryBytes = to params.query
    go !determinedParamsEncoder !remainingValueEncoders !nullabilities =
      case remainingValueEncoders of
        remainingValueEncodersHead : remainingValueEncodersTail ->
          tryError attempt >>= \case
            Right () -> goWithNullable
            Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23502" _ _ _ _))) ->
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
                  Statement.Statement queryBytes encoder decoder False
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
