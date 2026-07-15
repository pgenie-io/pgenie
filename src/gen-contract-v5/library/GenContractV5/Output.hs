-- |
-- The @Output@ half of gen-contract v5: what a Dhall generator's @compile@
-- function returns after being handed a "GenContractV5.Input" project —
-- either the generated 'File's (with any non-fatal 'Report' warnings), or
-- a fatal 'Report' explaining why generation failed.
module GenContractV5.Output
  ( Output (..),
    OutputOk (..),
    Report (..),
    File (..),
  )
where

import GenContractV4.Output (Report (..))
import GenContractV5.Output.File (File (..))
import GenContractV5.Output.Output (Output (..))
import GenContractV5.Output.OutputOk (OutputOk (..))
