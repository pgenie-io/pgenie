module CodegenAlgebra where

import Base.Prelude
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson

-- * Interfaces

data Codegen = forall configSection. Codegen
  { -- | Name of the config section.
    configSectionKey :: Text,
    -- | Major version of the codegen.
    version :: Int,
    -- | Specification of the parser of a section of the config file, where the section is identified by name.
    --
    -- TODO: Correct the signature.
    configSectionParser :: Aeson.Value -> Aeson.Parser configSection,
    generate :: configSection -> [QuerySignature] -> Either Error Artifact
  }

-- * Domain

data Artifact = Artifact
  { files :: [(FilePath, Text)]
  }

data Error

data QuerySignature
