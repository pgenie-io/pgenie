module Logic.SignatureFile
  ( Signature (..),
    FieldSig (..),
    ResultSig (..),
    Cardinality (..),
    signatureFilePath,
    fromInferred,
    serialize,
    tryParse,
    validateAndMerge,
    applyToQuery,
    cardinalityToGenInput,
  )
where

import AlgebraicPath qualified as Path
import Base.Prelude hiding (readFile, writeFile)
import Control.Foldl qualified as Fold
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Logic.Algebra qualified as Algebra
import PGenieGen.Model.Input qualified as Gen.Input
import YamlUnscrambler qualified as U

-- * Types

data Signature = Signature
  { parameters :: [(Text, FieldSig)],
    result :: Maybe ResultSig
  }
  deriving stock (Eq, Show)

data FieldSig = FieldSig
  { typeName :: Text,
    notNull :: Bool
  }
  deriving stock (Eq, Show)

data ResultSig = ResultSig
  { cardinality :: Cardinality,
    columns :: [(Text, FieldSig)]
  }
  deriving stock (Eq, Show)

data Cardinality
  = CardinalityZeroOrOne
  | CardinalityOne
  | CardinalityMany
  deriving stock (Eq, Show)

-- * Path computation

-- | Compute the signature file path from a query file path.
--
-- >>> signatureFilePath "queries/select_album_by_format.sql"
-- "queries/select_album_by_format.sig1.pgn.yaml"
signatureFilePath :: Path -> Path
signatureFilePath queryPath =
  Path.addExtension "sig1.pgn.yaml" (Path.dropExtensions queryPath)

-- * Conversion from inferred types

-- | Create a signature from inferred query types.
fromInferred :: [Gen.Input.Member] -> Maybe Gen.Input.ResultRows -> Signature
fromInferred params result =
  Signature
    { parameters = map memberToFieldEntry params,
      result = fmap resultRowsToResultSig result
    }
  where
    memberToFieldEntry :: Gen.Input.Member -> (Text, FieldSig)
    memberToFieldEntry member =
      ( member.pgName,
        FieldSig
          { typeName = valueToTypeName member.value,
            notNull = not member.isNullable
          }
      )

    resultRowsToResultSig :: Gen.Input.ResultRows -> ResultSig
    resultRowsToResultSig rr =
      ResultSig
        { cardinality = cardinalityFromGenInput rr.cardinality,
          columns = map memberToFieldEntry (toList rr.columns)
        }

-- * Cardinality conversion

cardinalityFromGenInput :: Gen.Input.ResultRowsCardinality -> Cardinality
cardinalityFromGenInput = \case
  Gen.Input.ResultRowsCardinalityOptional -> CardinalityZeroOrOne
  Gen.Input.ResultRowsCardinalitySingle -> CardinalityOne
  Gen.Input.ResultRowsCardinalityMultiple -> CardinalityMany

cardinalityToGenInput :: Cardinality -> Gen.Input.ResultRowsCardinality
cardinalityToGenInput = \case
  CardinalityZeroOrOne -> Gen.Input.ResultRowsCardinalityOptional
  CardinalityOne -> Gen.Input.ResultRowsCardinalitySingle
  CardinalityMany -> Gen.Input.ResultRowsCardinalityMultiple

-- * Value to type name

valueToTypeName :: Gen.Input.Value -> Text
valueToTypeName value =
  let baseName = scalarToTypeName value.scalar
      arraySuffix = case value.arraySettings of
        Nothing -> ""
        Just settings -> Text.replicate (fromIntegral settings.dimensionality) "[]"
   in baseName <> arraySuffix

scalarToTypeName :: Gen.Input.Scalar -> Text
scalarToTypeName = \case
  Gen.Input.ScalarPrimitive prim -> primitiveToTypeName prim
  Gen.Input.ScalarCustom name -> genNameToText name

primitiveToTypeName :: Gen.Input.Primitive -> Text
primitiveToTypeName = \case
  Gen.Input.PrimitiveBool -> "bool"
  Gen.Input.PrimitiveBytea -> "bytea"
  Gen.Input.PrimitiveChar -> "char"
  Gen.Input.PrimitiveCidr -> "cidr"
  Gen.Input.PrimitiveDate -> "date"
  Gen.Input.PrimitiveDatemultirange -> "datemultirange"
  Gen.Input.PrimitiveDaterange -> "daterange"
  Gen.Input.PrimitiveFloat4 -> "float4"
  Gen.Input.PrimitiveFloat8 -> "float8"
  Gen.Input.PrimitiveInet -> "inet"
  Gen.Input.PrimitiveInt2 -> "int2"
  Gen.Input.PrimitiveInt4 -> "int4"
  Gen.Input.PrimitiveInt4multirange -> "int4multirange"
  Gen.Input.PrimitiveInt4range -> "int4range"
  Gen.Input.PrimitiveInt8 -> "int8"
  Gen.Input.PrimitiveInt8multirange -> "int8multirange"
  Gen.Input.PrimitiveInt8range -> "int8range"
  Gen.Input.PrimitiveInterval -> "interval"
  Gen.Input.PrimitiveJson -> "json"
  Gen.Input.PrimitiveJsonb -> "jsonb"
  Gen.Input.PrimitiveMacaddr -> "macaddr"
  Gen.Input.PrimitiveMacaddr8 -> "macaddr8"
  Gen.Input.PrimitiveMoney -> "money"
  Gen.Input.PrimitiveNumeric -> "numeric"
  Gen.Input.PrimitiveNummultirange -> "nummultirange"
  Gen.Input.PrimitiveNumrange -> "numrange"
  Gen.Input.PrimitiveText -> "text"
  Gen.Input.PrimitiveTime -> "time"
  Gen.Input.PrimitiveTimestamp -> "timestamp"
  Gen.Input.PrimitiveTimestamptz -> "timestamptz"
  Gen.Input.PrimitiveTimetz -> "timetz"
  Gen.Input.PrimitiveTsmultirange -> "tsmultirange"
  Gen.Input.PrimitiveTsrange -> "tsrange"
  Gen.Input.PrimitiveTstzmultirange -> "tstzmultirange"
  Gen.Input.PrimitiveTstzrange -> "tstzrange"
  Gen.Input.PrimitiveUuid -> "uuid"
  Gen.Input.PrimitiveXml -> "xml"

genNameToText :: Gen.Input.Name -> Text
genNameToText name =
  let headText = wordToText (toList name.head)
      tailTexts = map wordOrNumberToText name.tail
   in Text.intercalate "_" (headText : tailTexts)
  where
    wordToText :: [Gen.Input.WordChar] -> Text
    wordToText = Text.pack . map wordCharToChar

    wordCharToChar :: Gen.Input.WordChar -> Char
    wordCharToChar = \case
      Gen.Input.WordCharA -> 'a'
      Gen.Input.WordCharB -> 'b'
      Gen.Input.WordCharC -> 'c'
      Gen.Input.WordCharD -> 'd'
      Gen.Input.WordCharE -> 'e'
      Gen.Input.WordCharF -> 'f'
      Gen.Input.WordCharG -> 'g'
      Gen.Input.WordCharH -> 'h'
      Gen.Input.WordCharI -> 'i'
      Gen.Input.WordCharJ -> 'j'
      Gen.Input.WordCharK -> 'k'
      Gen.Input.WordCharL -> 'l'
      Gen.Input.WordCharM -> 'm'
      Gen.Input.WordCharN -> 'n'
      Gen.Input.WordCharO -> 'o'
      Gen.Input.WordCharP -> 'p'
      Gen.Input.WordCharQ -> 'q'
      Gen.Input.WordCharR -> 'r'
      Gen.Input.WordCharS -> 's'
      Gen.Input.WordCharT -> 't'
      Gen.Input.WordCharU -> 'u'
      Gen.Input.WordCharV -> 'v'
      Gen.Input.WordCharW -> 'w'
      Gen.Input.WordCharX -> 'x'
      Gen.Input.WordCharY -> 'y'
      Gen.Input.WordCharZ -> 'z'

    wordOrNumberToText :: Gen.Input.WordOrNumber -> Text
    wordOrNumberToText = \case
      Gen.Input.WordOrNumberWord w -> wordToText (toList w)
      Gen.Input.WordOrNumberNumber n -> Text.pack (show n)

-- * Serialization

-- | Serialize a signature to YAML text.
serialize :: Signature -> Text
serialize sig =
  parametersSection <> resultSection
  where
    parametersSection =
      "parameters:"
        <> if null sig.parameters
          then " {}\n"
          else "\n" <> foldMap renderParam sig.parameters

    renderParam (name, field) =
      "  "
        <> name
        <> ":\n"
        <> "    type: "
        <> field.typeName
        <> "\n"
        <> "    not_null: "
        <> boolToText field.notNull
        <> "\n"

    resultSection = case sig.result of
      Nothing -> ""
      Just res ->
        "result:\n"
          <> "  cardinality: "
          <> cardinalityToText res.cardinality
          <> "\n"
          <> "  columns:\n"
          <> foldMap renderColumn res.columns

    renderColumn (name, field) =
      "    "
        <> name
        <> ":\n"
        <> "      type: "
        <> field.typeName
        <> "\n"
        <> "      not_null: "
        <> boolToText field.notNull
        <> "\n"

    boolToText True = "true"
    boolToText False = "false"

    cardinalityToText = \case
      CardinalityZeroOrOne -> "zero_or_one"
      CardinalityOne -> "one"
      CardinalityMany -> "many"

-- * Parsing

-- | Parse a signature from YAML text.
tryParse :: Text -> Either Text Signature
tryParse text =
  U.parseText signatureValue text
  where
    signatureValue :: U.Value Signature
    signatureValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ do
          parameters <-
            asum
              [ U.atByKey "parameters" parametersValue,
                pure []
              ]
          result <-
            asum
              [ fmap Just (U.atByKey "result" resultValue),
                pure Nothing
              ]
          pure Signature {parameters, result}

    parametersValue :: U.Value [(Text, FieldSig)]
    parametersValue =
      U.mappingValue
        $ U.foldMapping (,) Fold.list U.textString fieldSigValue

    fieldSigValue :: U.Value FieldSig
    fieldSigValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ do
          typeName <- U.atByKey "type" (U.scalarsValue [U.stringScalar U.textString])
          notNull <- U.atByKey "not_null" (U.scalarsValue [U.boolScalar])
          pure FieldSig {typeName, notNull}

    resultValue :: U.Value ResultSig
    resultValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ do
          cardinality <- U.atByKey "cardinality" cardinalityValue
          columns <- U.atByKey "columns" columnsValue
          pure ResultSig {cardinality, columns}

    cardinalityValue :: U.Value Cardinality
    cardinalityValue =
      U.scalarsValue [U.stringScalar cardinalityString]

    cardinalityString =
      U.formattedString "cardinality (zero_or_one, one, many)" $ \case
        "zero_or_one" -> Right CardinalityZeroOrOne
        "one" -> Right CardinalityOne
        "many" -> Right CardinalityMany
        other -> Left ("Invalid cardinality: " <> other <> ". Expected: zero_or_one, one, many")

    columnsValue :: U.Value [(Text, FieldSig)]
    columnsValue =
      U.mappingValue
        $ U.foldMapping (,) Fold.list U.textString fieldSigValue

-- * Validation

-- | Validate a file signature against an inferred signature.
-- Returns the merged signature with user refinements applied,
-- or an error if there's an invalid mismatch.
validateAndMerge ::
  -- | Inferred signature.
  Signature ->
  -- | File signature.
  Signature ->
  Either Algebra.Error Signature
validateAndMerge inferred file = do
  validatedParams <- validateFields "parameters" True inferred.parameters file.parameters
  validatedResult <- case (inferred.result, file.result) of
    (Nothing, Nothing) -> Right Nothing
    (Just _, Nothing) ->
      Left
        ( mismatchError
            "result"
            "Result section missing from signature file but query returns results"
        )
    (Nothing, Just _) ->
      Left
        ( mismatchError
            "result"
            "Result section present in signature file but query returns no results"
        )
    (Just inferredResult, Just fileResult) ->
      fmap Just (validateResult inferredResult fileResult)
  Right Signature {parameters = validatedParams, result = validatedResult}

validateFields ::
  -- | Section name for error messages.
  Text ->
  -- | Whether these are parameters (vs result columns).
  Bool ->
  -- | Inferred fields.
  [(Text, FieldSig)] ->
  -- | File fields.
  [(Text, FieldSig)] ->
  Either Algebra.Error [(Text, FieldSig)]
validateFields section isParam inferred file = do
  let inferredNames = Set.fromList (map fst inferred)
      fileNames = Set.fromList (map fst file)
  unless (inferredNames == fileNames) do
    Left
      ( mismatchError section
          $ "Field names mismatch. Inferred: "
          <> Text.intercalate ", " (map fst inferred)
          <> ". Signature file: "
          <> Text.intercalate ", " (map fst file)
      )
  let fileMap = Map.fromList file
  for inferred \(name, inf) -> do
    let fil = fromJust (Map.lookup name fileMap)
    validated <- validateField (section <> "/" <> name) isParam inf fil
    Right (name, validated)

validateResult :: ResultSig -> ResultSig -> Either Algebra.Error ResultSig
validateResult inferred file = do
  validatedColumns <- validateFields "result/columns" False inferred.columns file.columns
  -- Cardinality: user can change freely, use file's value.
  Right ResultSig {cardinality = file.cardinality, columns = validatedColumns}

validateField ::
  -- | Field path for error messages.
  Text ->
  -- | Is parameter (vs result column).
  Bool ->
  -- | Inferred.
  FieldSig ->
  -- | From file.
  FieldSig ->
  Either Algebra.Error FieldSig
validateField fieldPath isParam inferred file = do
  unless (inferred.typeName == file.typeName) do
    Left
      ( mismatchError fieldPath
          $ "Type mismatch. Inferred: "
          <> inferred.typeName
          <> ". Signature file: "
          <> file.typeName
      )
  case (inferred.notNull, file.notNull) of
    (True, False) ->
      Left
        ( mismatchError fieldPath
            $ "not_null constraint relaxed. Inferred: true. Signature file: false. "
            <> ( if isParam
                   then "Parameters"
                   else "Result columns"
               )
            <> " cannot have their not_null constraint relaxed"
        )
    _ ->
      Right file

mismatchError :: Text -> Text -> Algebra.Error
mismatchError fieldPath message =
  Algebra.Error
    { path = [],
      message = "Signature mismatch at " <> fieldPath <> ": " <> message,
      suggestion = Just "Update the signature file or fix the query",
      details = []
    }

-- * Applying refined signature to Gen.Input types

-- | Apply a validated signature to the Gen.Input types,
-- returning the refined parameters and result.
applyToQuery ::
  Signature ->
  [Gen.Input.Member] ->
  Maybe Gen.Input.ResultRows ->
  ([Gen.Input.Member], Maybe Gen.Input.ResultRows)
applyToQuery sig params result =
  ( zipWith applyFieldToMember (map snd sig.parameters) params,
    case (sig.result, result) of
      (Just sigResult, Just genResult) ->
        let updatedColumns =
              case nonEmpty (zipWith applyFieldToMember (map snd sigResult.columns) (toList genResult.columns)) of
                Nothing -> genResult.columns
                Just cols -> cols
         in Just
              Gen.Input.ResultRows
                { cardinality = cardinalityToGenInput sigResult.cardinality,
                  columns = updatedColumns
                }
      _ -> result
  )
  where
    applyFieldToMember :: FieldSig -> Gen.Input.Member -> Gen.Input.Member
    applyFieldToMember field member =
      Gen.Input.Member
        { name = member.name,
          pgName = member.pgName,
          isNullable = not field.notNull,
          value = member.value
        }
