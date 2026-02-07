{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module FsAlgebra.Runtimes.IO () where

import AlgebraicPath qualified as Path
import Base.Prelude
import Data.ByteString qualified as ByteString
import FsAlgebra.Algebra qualified as Algebra

instance (MonadIO m) => Algebra.ControlsFiles (ExceptT Algebra.Error m) where
  readFile path = do
    ExceptT do
      liftIO do
        catch (Right <$> ByteString.readFile (Path.toFilePath path)) \e ->
          case ioeGetErrorType e of
            NoSuchThing ->
              pure (Left (Algebra.FileNotFoundError path))
            _ ->
              error "TODO"
