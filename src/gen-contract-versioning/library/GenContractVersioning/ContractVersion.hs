-- |
-- The version identity of the Dhall generator contract, shared by every
-- rung of the backcompat chain and by the loader that checks a generator's
-- declared version against what this build implements.
module GenContractVersioning.ContractVersion
  ( ContractVersion (..),
  )
where

import Dhall qualified
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude

-- | Semantic version of the generator contract (the shape a Dhall generator
-- file must expose: @contractVersion@, @Config@, @compile@). Patch versions
-- are deliberately not represented: contract compatibility never depends on
-- them.
--
-- Compatibility rule: a generator declaring @(major, minor)@ is accepted by
-- a pgenie build iff the build implements the same @major@ and its
-- implemented @minor@ for that major is greater than or equal to the
-- declared one.
--
-- The direction of the minor check follows from who breaks on a mismatch.
-- Within a major the Input/Output Dhall types cannot change at all (the
-- loader typechecks the generator's @compile@ against the rung's exact
-- Dhall type), so a minor bump can only add semantics -- richer meaning in
-- Input the generator may rely on, or new Output conventions the build must
-- interpret. A generator declaring a newer minor than the build implements
-- may depend on semantics the build lacks, so it is rejected. A generator
-- declaring an older minor is exactly what minor bumps promise to keep
-- working, so it is accepted -- rejecting it would break every published
-- generator each time the contract ships a minor bump.
data ContractVersion = ContractVersion
  { major :: Natural,
    minor :: Natural
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)
