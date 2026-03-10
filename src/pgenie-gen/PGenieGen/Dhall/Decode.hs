module PGenieGen.Dhall.Decode where

import Dhall.Marshal.Decode
import Dhall.Pretty qualified
import PGenieGen.Prelude
import Prettyprinter qualified as Pp
import Prettyprinter.Render.Text qualified as Pp

-- | Narrow, restrict, rectify.
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

narrowWith :: (a -> Text) -> (a -> Maybe b) -> Decoder a -> Decoder b
narrowWith ontoError tryToOutput =
  constrain \a ->
    case tryToOutput a of
      Nothing -> Left (ontoError a)
      Just b -> Right b

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
      Pp.renderStrict (Pp.layoutCompact ("Can't narrow value: " <> Dhall.Pretty.prettyExpr expr))
