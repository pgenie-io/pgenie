module Infra.Adapters.Analyser.Sessions.Procedures.ResolveParamNullabilities
  ( ResolveParamNullabilities (..),
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as AttoparsecBs
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hasql.Errors qualified
import Hasql.Session qualified
import HasqlDev qualified
import Infra.Adapters.Analyser.Sessions.Algebras.Procedure
import Infra.Adapters.Analyser.Sessions.Domain
import Infra.Adapters.Analyser.Sessions.Procedures.ResolveParamNullabilities.DefaultTextualValue qualified as DefaultTextualValue
import Pqi qualified as Pqi
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
              Left err
                | isNotNullViolation err ->
                    -- When a not-null violation occurs with the current
                    -- parameter set to null, verify it is indeed caused by
                    -- _this_ parameter being null by retrying with a
                    -- non-null value. If that also triggers a 23502, the
                    -- violation originates elsewhere - most likely an
                    -- INSERT that omits a NOT NULL column that has no
                    -- DEFAULT.
                    tryError nonNullAttempt >>= \case
                      Right () -> goWithNonNullable
                      Left confirmedErr
                        | isNotNullViolation confirmedErr -> throwError confirmedErr
                        | isOtherConstraintViolation confirmedErr -> goWithNonNullable
                        | otherwise -> throwError confirmedErr
                | isOtherConstraintViolation err -> goWithNullable
                | otherwise -> throwError err
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

      -- The SQLSTATE code of a session error that originates from the server, if any.
      sqlStateCode :: Hasql.Errors.SessionError -> Maybe Text
      sqlStateCode = \case
        Hasql.Errors.StatementSessionError _ _ _ _ _ (Hasql.Errors.ServerStatementError (Hasql.Errors.ServerError code _ _ _ _)) -> Just code
        _ -> Nothing

      -- 23502: not_null_violation.
      isNotNullViolation :: Hasql.Errors.SessionError -> Bool
      isNotNullViolation err = sqlStateCode err == Just "23502"

      -- Any integrity-constraint violation (SQLSTATE class 23) other than
      -- not-null. E.g., a foreign-key column probed with a placeholder value
      -- absent from the referenced table (23503) - this is an artifact of
      -- the probe, not a signal about whether the tested parameter accepts
      -- @NULL@, so it is treated the same as a successful probe.
      isOtherConstraintViolation :: Hasql.Errors.SessionError -> Bool
      isOtherConstraintViolation err = case sqlStateCode err of
        Just code -> code /= "23502" && Text.isPrefixOf "23" code
        Nothing -> False

      executeAttempt :: [Maybe (Word32, ByteString, Pqi.Format)] -> Hasql.Session.Session ()
      executeAttempt parameterValues =
        Hasql.Session.onLibpqConnection \connection -> do
          result <- connection.execParams (encodeUtf8 params.query) parameterValues Pqi.Text
          case result of
            Nothing -> pure (Left (Hasql.Errors.DriverSessionError "execParams returned Nothing"), connection)
            Just result -> do
              status <- result.resultStatus
              case status of
                Pqi.CommandOk -> pure (Right (), connection)
                Pqi.EmptyQuery -> pure (Right (), connection)
                Pqi.TuplesOk -> pure (Right (), connection)
                Pqi.SingleTuple -> pure (Right (), connection)
                _ -> do
                  sessionError <- readSessionError result
                  pure (Left sessionError, connection)

      readSessionError :: Pqi.Result -> IO Hasql.Errors.SessionError
      readSessionError result = do
        code <- foldMap decodeUtf8Lenient <$> result.resultErrorField Pqi.DiagSqlstate
        message <- foldMap decodeUtf8Lenient <$> result.resultErrorField Pqi.DiagMessagePrimary
        detail <- fmap decodeUtf8Lenient <$> result.resultErrorField Pqi.DiagMessageDetail
        hint <- fmap decodeUtf8Lenient <$> result.resultErrorField Pqi.DiagMessageHint
        position <- mapMaybe parseInt <$> result.resultErrorField Pqi.DiagStatementPosition
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

      toLibpqParameters :: [Bool] -> [ByteString] -> [Maybe (Word32, ByteString, Pqi.Format)]
      toLibpqParameters nullabilities parameterBytes =
        zipWith
          ( \isNullable parameterByte ->
              if isNullable
                then Nothing
                else Just (0, parameterByte, Pqi.Text)
          )
          nullabilities
          parameterBytes
