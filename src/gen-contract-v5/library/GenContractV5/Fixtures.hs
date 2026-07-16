-- |
-- Public fixture module. Re-exports a selection of fixture values from the
-- internal "GenContractV5.Fixtures.Fixture1" module for use by test-suites
-- that depend on the @gen-contract-v5\@ library.
module GenContractV5.Fixtures
  ( input1,
  )
where

import GenContractV5.Contract qualified as Contract
import GenContractV5.Fixtures.Fixture1 qualified as Fixture1

-- | The first representative 'Contract.Project' value — a single-query,
-- single-migration "demo" project.
input1 :: Contract.Project
input1 = Fixture1.input
