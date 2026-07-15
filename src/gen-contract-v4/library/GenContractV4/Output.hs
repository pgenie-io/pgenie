-- |
-- The @Output@ half of gen-contract v4. Shape-identical to
-- "GenContractV5.Output" -- the v5 bump didn't touch Output/Report/File --
-- but kept as its own nominal type per rung, matching the rest of the
-- backcompat algebra.
module GenContractV4.Output
  ( Output (..),
    OutputOk (..),
    Report (..),
    File (..),
  )
where

import GenContractV4.Output.File (File (..))
import GenContractV4.Output.Output (Output (..))
import GenContractV4.Output.OutputOk (OutputOk (..))
import GenContractV4.Output.Report (Report (..))
