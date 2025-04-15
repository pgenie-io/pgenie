module Modelling.Procedures.ResolveTypeByOid
  ( Params (..),
    Result,
    Error (..),
    lifted,
    module Domain,
  )
where

import Base.Prelude hiding (Enum)
import HasqlDev
import Modelling.Procedures.ResolveTypeByOid.Algebras.Type qualified as Algebras.Type
import Modelling.Procedures.ResolveTypeByOid.Domain as Domain
import Modelling.Procedures.ResolveTypeByOid.Statements qualified as Statements
import SyntacticClass qualified as Syntactic
import TextBuilder qualified

data Params = Params
  { oid :: Word32
  }
  deriving stock (Show, Eq)

type Result = Type

data Error = Error
  { location :: [Text],
    reason :: Text
  }

type Location = [Text]

lifted ::
  ( RunsStatement m,
    MonadError Error m,
    MonadWriter [Error] m
  ) =>
  Params ->
  m Type
lifted (Params oid) =
  runReaderT (go oid) []
  where
    go oid =
      case Algebras.Type.maybeFromStandardOid oid of
        Just type_ -> pure type_
        Nothing -> do
          res <- lift $ runStatementByParams Statements.SelectTypeParams {oid = fromIntegral oid}
          type_ <- case res of
            Nothing -> crash ("Unknown type OID: " <> Syntactic.toText oid)
            Just type_ -> pure type_
          inContext ("type:" <> type_.name) do
            case type_.elementTypeOid of
              0 -> case type_.type_ of
                "b" -> crash "Base types are not supported"
                "c" -> do
                  fields <- lift $ runStatementByParams Statements.SelectAttributesParams {oid = fromIntegral oid}
                  fields <- forM fields $ \(Statements.SelectAttributesResultRow fieldName fieldTypeOid (fromIntegral -> fieldDims) _) ->
                    inContext ("field:" <> fieldName) do
                      fieldType <- go (fromIntegral fieldTypeOid)
                      fieldType <- case fieldDims of
                        0 -> pure fieldType
                        _ -> case fieldType of
                          Type existingDims elementScalar ->
                            if existingDims /= fieldDims
                              then do
                                warnStitching ["Replacing dimensionality from ", Syntactic.toTextBuilder existingDims, " to ", Syntactic.toTextBuilder fieldDims]
                                pure (Type fieldDims elementScalar)
                              else pure fieldType
                      pure (CompositeField fieldName fieldType)
                  pure (Type 0 (CompositeScalar (Composite type_.name fields)))
                "d" -> crashStitching ["Domain types are not supported yet"]
                "e" -> do
                  labels <- lift $ runStatementByParams Statements.SelectEnumLabelsParams {oid = fromIntegral oid}
                  pure (Type 0 (EnumScalar (Enum type_.name labels)))
                "p" -> crashStitching ["Pseudo types are not supported yet"]
                "r" -> crashStitching ["Range types are not supported yet"]
                "m" -> crashStitching ["Multirange types are not supported yet"]
                _ -> crashStitching ["Unexpected tag: ", TextBuilder.text type_.type_]
              _ -> inContext "element" do
                elementType <- go (fromIntegral type_.elementTypeOid)
                if elementType.dimensionality == 0
                  then pure elementType {dimensionality = 1}
                  else do
                    warnStitching ["Dimensionality is already set"]
                    pure elementType

-- * Supporting algebra

inContext :: (MonadReader Location m) => Text -> m a -> m a
inContext context = local (context :)

warn :: (MonadReader Location m, MonadWriter [Error] m) => Text -> m ()
warn reason = do
  location <- ask
  tell [Error location reason]

warnStitching :: (MonadReader Location m, MonadWriter [Error] m) => [TextBuilder] -> m ()
warnStitching =
  warn . TextBuilder.toText . mconcat

crash :: (MonadReader Location m, MonadError Error m) => Text -> m a
crash reason = do
  location <- ask
  throwError (Error location reason)

crashStitching :: (MonadReader Location m, MonadError Error m) => [TextBuilder] -> m a
crashStitching =
  crash . TextBuilder.toText . mconcat
