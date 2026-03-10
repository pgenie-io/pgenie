-- | Contract version type - generated from the Dhall contract definition.
module PGenieGen.Contract where

import Dhall.TH (GenerateOptions (..), HaskellType (..), defaultGenerateOptions, makeHaskellTypes, makeHaskellTypesWith)
import PGenieGen.Prelude

-- | Contract version with major and minor components.
-- Generated from @src/pgenie-gen/dhall/ContractVersion.dhall@.
makeHaskellTypes
  [ SingleConstructor "ContractVersion" "ContractVersion" "./src/pgenie-gen/dhall/ContractVersion.dhall"
  ]

deriving instance Show ContractVersion

deriving instance Eq ContractVersion
