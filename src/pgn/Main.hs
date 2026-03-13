module Main (main) where

import Base.Prelude
import Data.Text qualified as Text
import Infra qualified
import Paths_pgenie qualified as Paths
import Ui qualified

main :: IO ()
main =
  Ui.main semVer Infra.run

-- |
-- The SemVer version string derived from the package version (which follows PVP
-- with a leading @0.@ prefix).  E.g. cabal version @0.1.2.3@ becomes @1.2.3@.
semVer :: Text
semVer = case versionBranch Paths.version of
  0 : rest@(_ : _) -> Text.pack (intercalate "." (map show rest))
  branch -> Text.pack (intercalate "." (map show branch))
