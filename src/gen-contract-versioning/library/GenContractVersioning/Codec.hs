-- |
-- The 'Codec' the chain-walking dispatcher composes across
-- 'GenContractVersioning.PreviousVersionOf' hops.
module GenContractVersioning.Codec
  ( Codec (..),
    identityCodec,
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Dhall qualified
import Dhall.Core (Expr)
import Dhall.Src (Src)
import Prelude

-- | Bidirectional conversion between a rung's @input@\/@output@ Haskell
-- types and the Dhall expression the dispatcher operates on. A plain
-- function pair rather than a 'Dhall.Encoder'\/'Dhall.Decoder' pair: once
-- composed across a hop, encoding can fail
-- (@'GenContractVersioning.downgradeInput'@ can lose representability going
-- to an older rung), which a 'Dhall.Encoder' has no way to express.
data Codec input output = Codec
  { encode :: input -> Either Text (Expr Src Void),
    decode :: Expr Src Void -> Either Text output
  }

-- | The codec for a rung whose Dhall representation is exactly its own
-- Haskell type -- the base every chain is built from before
-- 'GenContractVersioning.downgradeInput'\/'GenContractVersioning.upgradeOutput'
-- get composed in.
identityCodec :: (Dhall.ToDhall input, Dhall.FromDhall output) => Codec input output
identityCodec =
  Codec
    { encode = Right . Dhall.inject.embed,
      decode = \expr -> case Dhall.rawInput Dhall.auto expr of
        Nothing -> Left "Failed to decode a Dhall expression into its expected Haskell type."
        Just output -> Right output
    }
