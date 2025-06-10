{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module FsLogic.Adapters.IO () where

import Base.Prelude
import FsLogic.Algebra qualified as Algebra

instance Algebra.ControlsFiles IO where
  catchSome action handler =
    catch action \ioe ->
      case maybeFrom @IOError ioe of
        Nothing -> do
          -- If the error is not recognized, rethrow it.
          throwIO ioe
        Just err -> do
          -- If the error is recognized, handle it.
          result <- handler err
          case result of
            Nothing -> throwIO ioe
            Just value -> pure value

instance IsSome IOError Algebra.Error where
  to = \case
    Algebra.FileNotFoundError path ->
      mkIOError NoSuchThing (to path) Nothing Nothing
    _ ->
      error "TODO"
  maybeFrom ioe =
    case ioeGetErrorType ioe of
      NoSuchThing -> do
        path <- ioeGetFileName ioe
        path <- maybeFrom path
        pure (Algebra.FileNotFoundError path)
      _ ->
        error "TODO"

-- FIXME: We cannot actually discern between FileNotFoundError and DirectoryNotFoundError. We need to handle in the operations instead and hence need ExceptT or similar.
