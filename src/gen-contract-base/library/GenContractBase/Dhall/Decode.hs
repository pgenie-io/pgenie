-- |
-- Combinators for building Dhall 'Decoder's that refine an existing decoder
-- with extra, Haskell-side validation instead of widening the Dhall type.
module GenContractBase.Dhall.Decode
  ( constrain,
    narrowWith,
    narrow,
  )
where

import Dhall.Marshal.Decode
import Dhall.Pretty qualified
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified
import Utils.Prelude

-- | Refine a 'Decoder' with a validation function. The error message
-- returned by the validation function becomes the Dhall decode failure.
constrain :: (a -> Either Text b) -> Decoder a -> Decoder b
constrain f Decoder {..} =
  Decoder
    { extract = \expr -> fromMonadic do
        a <- toMonadic (extract expr)
        case f a of
          Left err -> toMonadic (extractError err)
          Right b -> pure b,
      expected
    }

-- | Like 'narrow', but with a custom error message derived from the
-- rejected value instead of the default rendering of the Dhall expression.
narrowWith :: (a -> Text) -> (a -> Maybe b) -> Decoder a -> Decoder b
narrowWith ontoError tryToOutput =
  constrain \a ->
    case tryToOutput a of
      Nothing -> Left (ontoError a)
      Just b -> Right b

-- | Refine a 'Decoder' with a partial function. On failure, reports the
-- offending Dhall expression rendered as source.
narrow :: (a -> Maybe b) -> Decoder a -> Decoder b
narrow f Decoder {..} =
  Decoder
    { extract = \expr -> fromMonadic do
        a <- toMonadic (extract expr)
        case f a of
          Nothing -> toMonadic (extractError (renderExpr expr))
          Just b -> pure b,
      expected
    }
  where
    renderExpr expr =
      Prettyprinter.Render.Text.renderStrict (Prettyprinter.layoutCompact ("Can't narrow value: " <> Dhall.Pretty.prettyExpr expr))
