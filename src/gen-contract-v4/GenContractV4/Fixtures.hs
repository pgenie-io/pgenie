-- |
-- Public fixture module. Re-exports a selection of fixture values from the
-- internal "GenContractV4.Fixtures.Fixture1" module for use by test-suites
-- that depend on the @gen-contract-v4@ library.
module GenContractV4.Fixtures
  ( input1,
  )
where

import GenContractV4.Fixtures.Fixture1 qualified as Fixture1
import GenContractV4.Input qualified as Input

-- | The first representative 'Input.Project' value — a single-query,
-- single-migration "demo" project.
input1 :: Input.Project
input1 = Fixture1.input
