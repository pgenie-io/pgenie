-- |
-- Use LibPQ to extract the information about the query parameters and result.
module Modelling.Procedures.DescribeQuery
  ( Context,

    -- * Domain

    -- ** Params
    Params,

    -- ** Error
    Error (..),
    ErrorResult (..),

    -- ** Result
    Result (..),
    ResultColumn (..),

    -- * Execution
    lifted,
    io,
  )
where

import Base.Prelude
import Data.Attoparsec.ByteString.Char8 qualified as AttoparsecBs
import Data.Vector qualified as Vector
import Database.PostgreSQL.LibPQ qualified as Pq
import LibpqExtras.LawfulConversions ()

type Context =
  Pq.Connection

-- * Domain

data Params = Params
  { query :: Text
  }
  deriving stock (Show, Eq)

data Error
  = ConnectionError
  | ResultError ErrorResult
  deriving stock (Show, Eq)

instance IsSome Error Error where
  to = id
  maybeFrom = Just

data ErrorResult = ErrorResult
  { code :: Text,
    message :: Text,
    -- \* Associated offset in the associated query string
    position :: Maybe Int
  }
  deriving stock (Show, Eq)

data Result = Result
  { paramTypeOids :: Vector Word32,
    resultColumns :: Vector ResultColumn
  }
  deriving stock (Show, Eq)

data ResultColumn = ResultColumn
  { -- | Name if it's present and makes valid UTF-8.
    name :: Maybe Text,
    -- | Type OID.
    typeOid :: Word32,
    -- | Table OID. Absent when 0.
    tableOid :: Word32,
    -- | Index within the table. Absent when 0.
    tableColumnIndex :: Int32
  }
  deriving stock (Show, Eq)

-- * Lifted

-- | Execution with all things lifted.
--
-- Supposed to make it easier to embed in other monads.
lifted ::
  ( MonadIO m,
    MonadReader context m,
    MonadError error m,
    Has Pq.Connection context,
    IsSome error Error
  ) =>
  Params -> m Result
lifted params = do
  conn <- asks getter
  res <- liftIO $ io conn params
  liftEither $ first to res

-- * IO

-- | Specific execution.
io :: Pq.Connection -> Params -> IO (Either Error Result)
io conn params = runExceptT do
  res <- lift $ Pq.prepare conn "" (to params.query) Nothing
  res <- case res of
    Nothing -> throwError ConnectionError
    Just res -> return res
  status <- lift $ Pq.resultStatus res
  case status of
    Pq.CommandOk -> return ()
    Pq.FatalError -> lift (readResultErrorDetails res) >>= throwError . ResultError
    _ -> error ("Bug. Unexpected status: " <> show status)

  res <- lift $ Pq.describePrepared conn ""
  res <- case res of
    Nothing -> throwError ConnectionError
    Just res -> return res
  status <- lift $ Pq.resultStatus res
  case status of
    Pq.CommandOk -> return ()
    _ -> error ("Bug. Unexpected status: " <> show status)

  lift (Result <$> readParamTypeOids res <*> readResultColumns res)

-- * Helpers

readParamTypeOids :: Pq.Result -> IO (Vector Word32)
readParamTypeOids res = do
  amount <- Pq.nparams res
  Vector.generateM amount $ \i -> do
    fmap to $ Pq.paramtype res i

readResultColumns :: Pq.Result -> IO (Vector ResultColumn)
readResultColumns res = do
  amount <- fromIntegral . to @Int32 <$> Pq.nfields res
  Vector.generateM amount $ \i -> do
    let col = from @Int32 (fromIntegral i)
    name <- Pq.fname res col
    name <- pure case name of
      Nothing -> error "Oops! Trying to access a missing column"
      Just "?column?" -> Nothing
      Just name -> maybeFrom name
    typeOid <- fmap to $ Pq.ftype res col
    tableOid <- fmap to $ Pq.ftable res col
    tableCol <- fmap to $ Pq.ftablecol res col
    return $ ResultColumn name typeOid tableOid tableCol

readResultErrorDetails :: Pq.Result -> IO ErrorResult
readResultErrorDetails res = do
  code <- foldMap from <$> Pq.resultErrorField res Pq.DiagSqlstate
  message <- foldMap from <$> Pq.resultErrorField res Pq.DiagMessagePrimary
  position <- mapMaybe parseInt <$> Pq.resultErrorField res Pq.DiagStatementPosition
  pure ErrorResult {..}
  where
    parseInt :: ByteString -> Maybe Int
    parseInt byteString =
      byteString
        & AttoparsecBs.parseOnly (AttoparsecBs.decimal <* AttoparsecBs.endOfInput)
        & either (const Nothing) Just
