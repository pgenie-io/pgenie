module GenAlgebra where

import Base.Prelude
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import ProjectFile qualified

-- * Interfaces

data Gen = forall specificConfig. Gen
  { -- | Name of the config section.
    configSectionKey :: Text,
    -- | Major version of the codegen.
    version :: Int,
    -- | Specification of the parser of a section of the config file, where the section is identified by key and version.
    configSectionParser :: Aeson.Value -> Aeson.Parser specificConfig,
    generate :: ProjectFile.ProjectFile -> specificConfig -> [QuerySignature] -> Either Error Artifact
  }

-- * Domain

data Artifact = Artifact
  { files :: [(FilePath, Text)]
  }

data Error

data QuerySignature

-- * Ops

mergeSignatures :: QuerySignature -> QuerySignature -> Either Text QuerySignature
mergeSignatures =
  error "TODO"
