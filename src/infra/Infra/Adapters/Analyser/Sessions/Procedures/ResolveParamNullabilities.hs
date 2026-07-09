module Infra.Adapters.Analyser.Sessions.Procedures.ResolveParamNullabilities
  ( ResolveParamNullabilities (..),
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as AttoparsecBs
import Data.Vector qualified as Vector
import Database.PostgreSQL.LibPQ qualified as Pq
import Hasql.Errors qualified
import Hasql.Session qualified
import HasqlDev qualified
import Infra.Adapters.Analyser.Sessions.Algebras.Procedure
import Infra.Adapters.Analyser.Sessions.Domain
import Infra.Adapters.Analyser.Sessions.Procedures.ResolveParamNullabilities.DefaultTextualValue qualified as DefaultTextualValue
import SyntacticClass qualified as Syntactic
import Utils.Prelude

-- |
-- Resolves, per parameter, whether the database will accept @NULL@ for it.
-- Determined empirically: each parameter is probed with a real query
-- execution rather than derived from static catalog information, since
-- Postgres does not expose parameter nullability directly.
data ResolveParamNullabilities = ResolveParamNullabilities
  { query :: Text,
    paramTypes :: Vector Type
  }
  deriving stock (Show, Eq)

instance IsProcedure ResolveParamNullabilities where
  type ProcedureResult ResolveParamNullabilities = Vector Param
  runProcedure params = do
    parameterBytes <- Vector.iforM params.paramTypes \index type_ ->
      inContext
        ["param:", Syntactic.toTextBuilder (show index)]
        case DefaultTextualValue.fromType type_ of
          Nothing ->
            crash
              ["Unsupported type"]
              [ ("type", Syntactic.toText (show type_))
              ]
          Just text -> pure (encodeUtf8 text)
    let parameterBytesList = Vector.toList parameterBytes
    nullabilities <- Vector.fromList <$> HasqlDev.runSession (go parameterBytesList [] parameterBytesList [])
    return
      ( Vector.zipWith
          (\type_ nullable -> Param {nullable, type_})
          params.paramTypes
          nullabilities
      )
    where
      go !allParameterBytes !determinedNullabilities !remainingParameterBytes !nullabilities =
        case remainingParameterBytes of
          _remainingParameterBytesHead : remainingParameterBytesTail ->
            tryError attempt >>= \case
              Right () -> goWithNullable
              Left (Hasql.Errors.StatementSessionError _ _ _ _ _ (Hasql.Errors.ServerStatementError (Hasql.Errors.ServerError "23502" _ _ _ _))) ->
                -- When a not-null violation occurs with the current parameter
                -- set to null, verify it is indeed caused by *this* parameter
                -- being null by retrying with a non-null value. If that also
                -- triggers a 23502, the violation originates elsewhere - most
                -- likely an INSERT that omits a NOT NULL column that has no
                -- DEFAULT.
                tryError nonNullAttempt >>= \case
                  Right () -> goWithNonNullable
                  Left confirmedErr -> throwError confirmedErr
              Left err -> throwError err
            where
              goWithNullable =
                go
                  allParameterBytes
                  (True : determinedNullabilities)
                  remainingParameterBytesTail
                  (True : nullabilities)
              goWithNonNullable =
                go
                  allParameterBytes
                  (False : determinedNullabilities)
                  remainingParameterBytesTail
                  (False : nullabilities)
              attempt =
                executeAttempt
                  (toLibpqParameters (reverse determinedNullabilities ++ [True] ++ replicate (length remainingParameterBytesTail) False) allParameterBytes)
              nonNullAttempt =
                executeAttempt
                  (toLibpqParameters (reverse determinedNullabilities ++ [False] ++ replicate (length remainingParameterBytesTail) False) allParameterBytes)
          [] -> return $ reverse nullabilities

      executeAttempt :: [Maybe (Pq.Oid, ByteString, Pq.Format)] -> Hasql.Session.Session ()
      executeAttempt parameterValues =
        Hasql.Session.onLibpqConnection \connection -> do
          result <- Pq.execParams connection (encodeUtf8 params.query) parameterValues Pq.Text
          case result of
            Nothing -> pure (Left (Hasql.Errors.DriverSessionError "libpq execParams returned Nothing"), connection)
            Just result -> do
              status <- Pq.resultStatus result
              case status of
                Pq.CommandOk -> pure (Right (), connection)
                Pq.EmptyQuery -> pure (Right (), connection)
                Pq.TuplesOk -> pure (Right (), connection)
                Pq.SingleTuple -> pure (Right (), connection)
                _ -> do
                  sessionError <- readSessionError result
                  pure (Left sessionError, connection)

      readSessionError :: Pq.Result -> IO Hasql.Errors.SessionError
      readSessionError result = do
        code <- foldMap decodeUtf8Lenient <$> Pq.resultErrorField result Pq.DiagSqlstate
        message <- foldMap decodeUtf8Lenient <$> Pq.resultErrorField result Pq.DiagMessagePrimary
        detail <- fmap decodeUtf8Lenient <$> Pq.resultErrorField result Pq.DiagMessageDetail
        hint <- fmap decodeUtf8Lenient <$> Pq.resultErrorField result Pq.DiagMessageHint
        position <- mapMaybe parseInt <$> Pq.resultErrorField result Pq.DiagStatementPosition
        pure
          ( Hasql.Errors.StatementSessionError
              0
              0
              params.query
              []
              False
              (Hasql.Errors.ServerStatementError (Hasql.Errors.ServerError code message detail hint position))
          )
        where
          parseInt :: ByteString -> Maybe Int
          parseInt bs =
            bs
              & AttoparsecBs.parseOnly (AttoparsecBs.decimal <* AttoparsecBs.endOfInput)
              & either (const Nothing) Just

      toLibpqParameters :: [Bool] -> [ByteString] -> [Maybe (Pq.Oid, ByteString, Pq.Format)]
      toLibpqParameters nullabilities parameterBytes =
        zipWith
          ( \isNullable parameterByte ->
              if isNullable
                then Nothing
                else Just (Pq.invalidOid, parameterByte, Pq.Text)
          )
          nullabilities
          parameterBytes
