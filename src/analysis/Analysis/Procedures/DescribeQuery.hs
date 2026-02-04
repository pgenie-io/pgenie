module Analysis.Procedures.DescribeQuery
  ( DescribeQuery (..),
    DescribeQueryResult (..),
    DescribeQueryResultColumn (..),
  )
where

import Analysis.Algebras.Procedure
import Base.Prelude
import Data.Vector qualified as Vector
import Hasql.Session qualified
import HasqlDev qualified
import LibpqExtras.Procedures.DescribeQuery qualified
import SyntacticClass qualified as Syntactic

data DescribeQuery = DescribeQuery
  { query :: Text
  }
  deriving stock (Show, Eq)

data DescribeQueryResult = DescribeQueryResult
  { paramTypeOids :: Vector Word32,
    resultColumns :: Vector DescribeQueryResultColumn
  }
  deriving stock (Show, Eq)

data DescribeQueryResultColumn = DescribeQueryResultColumn
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

instance Procedure DescribeQuery where
  type ProcedureResult DescribeQuery = DescribeQueryResult
  runProcedure (DescribeQuery query) = do
    result <- HasqlDev.runSession do
      Hasql.Session.onLibpqConnection \libpqConnection -> do
        result <-
          LibpqExtras.Procedures.DescribeQuery.io
            libpqConnection
            LibpqExtras.Procedures.DescribeQuery.Params {query}
        pure (Right result, libpqConnection)
    case result of
      Right (LibpqExtras.Procedures.DescribeQuery.Result paramTypeOids resultColumns) ->
        pure
          DescribeQueryResult
            { paramTypeOids,
              resultColumns =
                Vector.map
                  ( \(LibpqExtras.Procedures.DescribeQuery.ResultColumn name typeOid typeMod tableOid tableColumnIndex) ->
                      DescribeQueryResultColumn {name, typeOid, typeMod, tableOid, tableColumnIndex}
                  )
                  resultColumns
            }
      Left error -> case error of
        LibpqExtras.Procedures.DescribeQuery.ConnectionError ->
          crash ["Connection error"]
        LibpqExtras.Procedures.DescribeQuery.ResultError code message position ->
          crash
            [ "Broken query. SQLSTATE code: ",
              Syntactic.toTextBuilder code,
              ", message: ",
              Syntactic.toTextBuilder message,
              maybe
                ""
                (\pos -> ", position: " <> Syntactic.toTextBuilder pos)
                position
            ]
