-- |
-- The @Output@ half of gen-contract v5: what a Dhall generator's @compile@
-- function returns after being handed a "GenContractV5.Input" project —
-- either the generated 'File's (with any non-fatal 'Report' warnings), or
-- a fatal 'Report' explaining why generation failed.
--
-- Shape-identical to "GenContractV4.Output" -- the v5 bump didn't touch
-- Output/OutputOk/File/Report -- so re-exported wholesale rather than
-- redefined as a nominally distinct rung.
module GenContractV5.Output
  ( Output (..),
    OutputOk (..),
    Report (..),
    File (..),
  )
where

import GenContractV4.Output (File (..), Output (..), OutputOk (..), Report (..))
