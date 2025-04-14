{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module Modelling.Procedures.ResolveTypeByOid where

import Base.Prelude hiding (Enum)
import HasqlDev
import Modelling.Domain
import Modelling.Procedures.ResolveTypeByOid.Logic qualified as Logic
import Modelling.Procedures.ResolveTypeByOid.Statements qualified as Statements

effect ::
  ( RunsStatement m,
    MonadError Error m
  ) =>
  Word32 ->
  m Type
effect oid = do
  case Logic.maybeTypeFromStandardOid oid of
    Just type_ -> return type_
    Nothing -> do
      res <- runStatementByParams Statements.SelectTypeParams {oid = fromIntegral oid}
      case res of
        Nothing -> error ("Unknown type OID: " <> show oid)
        Just type_ ->
          case type_.elementTypeOid of
            0 -> case type_.type_ of
              "b" -> throwError $ UnsupportedFeature "Base type" ""
              _ -> error "TODO"
            _ -> Logic.liftTypeToArrayType <$> effect (fromIntegral type_.elementTypeOid)
