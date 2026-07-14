-- | Re-export of the latest gen-contract rung's @Input@ half. See
-- "GenContractV5.Input" for the actual definitions — this module exists so
-- every existing importer (@GenBridge.Model.Input qualified as Gen.Input@,
-- used throughout @logic@/@infra@) keeps compiling unchanged as rungs come
-- and go.
module GenBridge.Model.Input
  ( module GenContractV5.Input,
  )
where

import GenContractV5.Input
