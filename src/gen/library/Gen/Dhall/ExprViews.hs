-- |
-- Narrow views into parsed Dhall expressions, used to pull specific fields
-- out of a generator's top-level record without fully decoding it.
module Gen.Dhall.ExprViews
  ( recordField,
  )
where

import Dhall.Core
import Dhall.Map qualified
import Utils.Prelude

-- | Look up a field in a Dhall record literal by name.
recordField :: Text -> Expr s a -> Maybe (Expr s a)
recordField fieldName = \case
  RecordLit fields -> recordFieldValue <$> Dhall.Map.lookup fieldName fields
  _ -> Nothing
