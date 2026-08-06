-- |
-- Use the wire-protocol Describe message to extract the information about
-- the query parameters and result.
module Infra.Adapters.Analyser.Sessions.LibpqExtras.Procedures.DescribeQuery
  ( Context,

    -- * Domain

    -- ** Params
    Params (..),

    -- ** Error
    Error (..),

    -- ** Result
    Result (..),
    ResultColumn (..),

    -- * Execution
    io,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as AttoparsecBs
import Data.Vector qualified as Vector
import Pqi qualified as Pqi
import Utils.Prelude

-- | The connection a description is run against.
type Context =
  Pqi.Connection

-- * Domain

data Params = Params
  { query :: Text
  }
  deriving stock (Show, Eq)

data Error
  = ConnectionError
  | ResultError
      -- | SQLSTATE code.
      Text
      -- | Message.
      Text
      -- | Offset in the associated query string.
      (Maybe Int)
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

-- * IO

-- | Specific execution.
io :: Pqi.Connection -> Params -> IO (Either Error Result)
io conn params = runExceptT do
  res <- lift $ conn.prepare "" (encodeUtf8 params.query) Nothing
  res <- case res of
    Nothing -> throwError ConnectionError
    Just res -> return res
  status <- lift res.resultStatus
  case status of
    Pqi.CommandOk -> return ()
    Pqi.FatalError -> lift (readResultErrorDetails res) >>= throwError
    _ -> error ("Bug. Unexpected status: " <> show status)

  res <- lift $ conn.describePrepared ""
  res <- case res of
    Nothing -> throwError ConnectionError
    Just res -> return res
  status <- lift res.resultStatus
  case status of
    Pqi.CommandOk -> return ()
    _ -> error ("Bug. Unexpected status: " <> show status)

  lift (Result <$> readParamTypeOids res <*> readResultColumns res)
  where
    readParamTypeOids :: Pqi.Result -> IO (Vector Word32)
    readParamTypeOids res = do
      amount <- res.nparams
      Vector.generateM (fromIntegral amount) $ \i ->
        res.paramtype (fromIntegral i)

    readResultColumns :: Pqi.Result -> IO (Vector ResultColumn)
    readResultColumns res = do
      amount <- res.nfields
      Vector.generateM (fromIntegral amount) $ \i -> do
        let col = fromIntegral i :: Int32
        name <- res.fname col
        name <- pure case name of
          Nothing -> error "Oops! Trying to access a missing column"
          Just "?column?" -> Nothing
          Just name -> either (const Nothing) Just (decodeUtf8 name)
        typeOid <- res.ftype col
        typeMod <- res.fmod col
        tableOid <- res.ftable col
        tableCol <- res.ftablecol col
        return $ ResultColumn name typeOid typeMod tableOid tableCol

    readResultErrorDetails :: Pqi.Result -> IO Error
    readResultErrorDetails res = do
      code <- foldMap decodeUtf8Lenient <$> res.resultErrorField Pqi.DiagSqlstate
      message <- foldMap decodeUtf8Lenient <$> res.resultErrorField Pqi.DiagMessagePrimary
      position <- mapMaybe parseInt <$> res.resultErrorField Pqi.DiagStatementPosition
      pure (ResultError code message position)
      where
        parseInt :: ByteString -> Maybe Int
        parseInt byteString =
          byteString
            & AttoparsecBs.parseOnly (AttoparsecBs.decimal <* AttoparsecBs.endOfInput)
            & either (const Nothing) Just
