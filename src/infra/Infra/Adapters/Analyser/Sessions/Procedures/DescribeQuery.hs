module Infra.Adapters.Analyser.Sessions.Procedures.DescribeQuery
  ( DescribeQuery (..),
    DescribeQueryResult (..),
    DescribeQueryResultColumn (..),
  )
where

import Base.Prelude
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Hasql.Session qualified
import HasqlDev qualified
import Infra.Adapters.Analyser.Sessions.Algebras.Procedure
import Infra.Adapters.Analyser.Sessions.LibpqExtras.Procedures.DescribeQuery qualified as LibpqExtras
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
          LibpqExtras.io
            libpqConnection
            LibpqExtras.Params {query}
        pure (Right result, libpqConnection)
    case result of
      Right (LibpqExtras.Result paramTypeOids resultColumns) ->
        pure
          DescribeQueryResult
            { paramTypeOids,
              resultColumns =
                Vector.map
                  ( \(LibpqExtras.ResultColumn name typeOid typeMod tableOid tableColumnIndex) ->
                      DescribeQueryResultColumn {name, typeOid, typeMod, tableOid, tableColumnIndex}
                  )
                  resultColumns
            }
      Left error -> case error of
        LibpqExtras.ConnectionError ->
          crash ["Connection error"] []
        LibpqExtras.ResultError code message position ->
          crash
            ["Broken query"]
            ( catMaybes
                [ Just ("code", Syntactic.toText code),
                  Just ("message", Syntactic.toText message),
                  case position of
                    Nothing -> Just ("sql", query)
                    Just pos -> Just ("sql", formatQueryWithErrorPointer query pos)
                ]
            )
    where
      formatQueryWithErrorPointer :: Text -> Int -> Text
      formatQueryWithErrorPointer queryText errorPos =
        let queryLines = Text.lines queryText
            (lineIdx, columnIdx) = findErrorLocation queryLines errorPos
            linesWithPointer = insertPointerAfterLine queryLines lineIdx columnIdx
         in Text.unlines linesWithPointer
        where
          findErrorLocation :: [Text] -> Int -> (Int, Int)
          findErrorLocation lines errorPos = go lines 1 0
            where
              go [] currentPos _ = (length lines, 1)
              go (line : rest) currentPos lineIdx =
                let lineLength = Text.length line
                    lineEnd = currentPos + lineLength
                 in if errorPos <= lineEnd
                      then (lineIdx, errorPos - currentPos + 1)
                      else go rest (lineEnd + 1) (lineIdx + 1) -- +1 for newline
          insertPointerAfterLine :: [Text] -> Int -> Int -> [Text]
          insertPointerAfterLine lines lineIdx columnIdx =
            let (before, after) = splitAt lineIdx lines
                pointer = Syntactic.toText (replicate (max 0 (columnIdx - 1)) ' ') <> "^"
             in case after of
                  [] -> before <> [pointer]
                  (errorLine : rest) -> before <> [errorLine, pointer] <> rest
