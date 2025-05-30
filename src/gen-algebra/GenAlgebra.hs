module GenAlgebra where

import Base.Prelude
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson

-- * Interfaces

data Gen = forall generatorConfig. Gen
  { -- | Name of the config section.
    configSectionKey :: Text,
    -- | Major version of the codegen.
    version :: Int,
    -- | Specification of the parser of a section of the config file, where the section is identified by key and version.
    generatorConfigParser :: Aeson.Value -> Aeson.Parser generatorConfig,
    defaultGeneratorConfig :: generatorConfig,
    generate :: generatorConfig -> Project -> Either Error Artifact
  }

-- * Domain

data Artifact = Artifact
  { files :: [(FilePath, Text)]
  }

data Error

data Project = Project
  { name :: Text,
    version :: NonEmpty Word,
    queries :: [Query]
  }

data Query = Query
  { name :: Name,
    queryTemplate :: QueryTemplate
  }

data QueryTemplate = QueryTemplate
  { parts :: [QueryTemplatePart],
    -- | Variables used in the query template in the order they appear.
    vars :: [Name]
  }

data QueryTemplatePart
  = SqlQueryTemplatePart Text
  | VarQueryTemplatePart Name

type Name = [Text]

-- * Ops

mergeQueries :: Query -> Query -> Either Text Query
mergeQueries =
  error "TODO"
