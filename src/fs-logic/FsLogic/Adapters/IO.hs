{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module FsLogic.Adapters.IO () where

import Base.Prelude
import Data.ByteString qualified as ByteString
import FsLogic.Algebra qualified as Algebra

instance (MonadIO m) => Algebra.ControlsFiles (ExceptT Algebra.Error m) where
  readFile path = do
    ExceptT do
      liftIO do
        catch (Right <$> ByteString.readFile (to path)) \e ->
          case ioeGetErrorType e of
            NoSuchThing ->
              pure (Left (Algebra.FileNotFoundError path))
            _ ->
              error "TODO"
  catchSome action handler =
    ExceptT do
      runExceptT action >>= \case
        Left err ->
          runExceptT (handler err) >>= \case
            Left handlerErr -> pure (Left handlerErr)
            Right Nothing -> pure (Left err)
            Right (Just value) -> pure (Right value)
        Right value -> pure (Right value)
