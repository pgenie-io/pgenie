module QueryFormatAlgebra where

import Base.Prelude

data QueryFormat = QueryFormat
  { -- | Expect files with these extensions to be read.
    fileExtensions :: [Text],
    -- | Try to process the provided query producing SQL.
    compileQuery :: Text -> Either Text Text
  }
