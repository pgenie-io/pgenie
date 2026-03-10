module PGenieGen.Dhall.ExprViews where

import Dhall.Core
import Dhall.Map qualified as Map
import PGenieGen.Prelude

recordField :: Text -> Expr s a -> Maybe (Expr s a)
recordField fieldName = \case
  RecordLit fields -> recordFieldValue <$> Map.lookup fieldName fields
  _ -> Nothing
