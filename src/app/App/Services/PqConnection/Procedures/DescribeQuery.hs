module App.Services.PqConnection.Procedures.DescribeQuery
  ( -- * Params
    Params (..),

    -- * Result
    Result (..),
    ResultColumn (..),
  )
where

import App.Frameworks.Service
import App.Services.PqConnection.Context qualified as Context
import App.Services.PqConnection.Utils.LawfulConversions ()
import Base.Prelude
import Data.Attoparsec.ByteString.Char8 qualified as AttoparsecBs
import Data.Vector qualified as Vector
import Database.PostgreSQL.LibPQ qualified as Pq

data Params = Params
  { query :: Text
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
    -- | Type modifier. The interpretation of modifier values is type-specific; they typically indicate precision or size limits. The value -1 is used to indicate "no information available". Most data types do not use modifiers, in which case the value is always -1.
    typeMod :: Int,
    -- | Table OID. Absent when 0.
    tableOid :: Word32,
    -- | Index within the table. Absent when 0.
    tableColumnIndex :: Int32
  }
  deriving stock (Show, Eq)

instance IsProcedure Params where
  type ProcedureContext Params = Context.Context
  type ProcedureResult Params = Result
  proceed (Context.Context conn) _ params = runExceptT do
    res <- lift $ Pq.prepare conn "" (to params.query) Nothing
    res <- case res of
      Nothing -> throwError Context.ConnectionError
      Just res -> return res
    status <- lift $ Pq.resultStatus res
    case status of
      Pq.CommandOk -> return ()
      Pq.FatalError -> lift (readResultErrorDetails res) >>= throwError
      _ -> error ("Bug. Unexpected status: " <> show status)

    res <- lift $ Pq.describePrepared conn ""
    res <- case res of
      Nothing -> throwError Context.ConnectionError
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
    typeMod <- Pq.fmod res col
    tableOid <- fmap to $ Pq.ftable res col
    tableCol <- fmap to $ Pq.ftablecol res col
    return $ ResultColumn name typeOid typeMod tableOid tableCol

readResultErrorDetails :: Pq.Result -> IO (Context.Error Context.Context)
readResultErrorDetails res = do
  code <- foldMap from <$> Pq.resultErrorField res Pq.DiagSqlstate
  message <- foldMap from <$> Pq.resultErrorField res Pq.DiagMessagePrimary
  position <- mapMaybe parseInt <$> Pq.resultErrorField res Pq.DiagStatementPosition
  pure (Context.ResultError code message position)
  where
    parseInt :: ByteString -> Maybe Int
    parseInt byteString =
      byteString
        & AttoparsecBs.parseOnly (AttoparsecBs.decimal <* AttoparsecBs.endOfInput)
        & either (const Nothing) Just
