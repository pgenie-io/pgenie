module PGenieGen.Model.Input.WordChar where

import AesonDeriver qualified
import Dhall qualified
import PGenieGen.Dhall.Deriving qualified as Dhall.Deriving
import PGenieGen.Dhall.Orphans ()
import PGenieGen.Prelude

data WordChar
  = WordCharA
  | WordCharB
  | WordCharC
  | WordCharD
  | WordCharE
  | WordCharF
  | WordCharG
  | WordCharH
  | WordCharI
  | WordCharJ
  | WordCharK
  | WordCharL
  | WordCharM
  | WordCharN
  | WordCharO
  | WordCharP
  | WordCharQ
  | WordCharR
  | WordCharS
  | WordCharT
  | WordCharU
  | WordCharV
  | WordCharW
  | WordCharX
  | WordCharY
  | WordCharZ
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "WordChar") WordChar)

AesonDeriver.derive
  [ ''WordChar
  ]

toChar :: WordChar -> Char
toChar = \case
  WordCharA -> 'a'
  WordCharB -> 'b'
  WordCharC -> 'c'
  WordCharD -> 'd'
  WordCharE -> 'e'
  WordCharF -> 'f'
  WordCharG -> 'g'
  WordCharH -> 'h'
  WordCharI -> 'i'
  WordCharJ -> 'j'
  WordCharK -> 'k'
  WordCharL -> 'l'
  WordCharM -> 'm'
  WordCharN -> 'n'
  WordCharO -> 'o'
  WordCharP -> 'p'
  WordCharQ -> 'q'
  WordCharR -> 'r'
  WordCharS -> 's'
  WordCharT -> 't'
  WordCharU -> 'u'
  WordCharV -> 'v'
  WordCharW -> 'w'
  WordCharX -> 'x'
  WordCharY -> 'y'
  WordCharZ -> 'z'

maybeFromChar :: Char -> Maybe WordChar
maybeFromChar = \case
  'a' -> Just WordCharA
  'b' -> Just WordCharB
  'c' -> Just WordCharC
  'd' -> Just WordCharD
  'e' -> Just WordCharE
  'f' -> Just WordCharF
  'g' -> Just WordCharG
  'h' -> Just WordCharH
  'i' -> Just WordCharI
  'j' -> Just WordCharJ
  'k' -> Just WordCharK
  'l' -> Just WordCharL
  'm' -> Just WordCharM
  'n' -> Just WordCharN
  'o' -> Just WordCharO
  'p' -> Just WordCharP
  'q' -> Just WordCharQ
  'r' -> Just WordCharR
  's' -> Just WordCharS
  't' -> Just WordCharT
  'u' -> Just WordCharU
  'v' -> Just WordCharV
  'w' -> Just WordCharW
  'x' -> Just WordCharX
  'y' -> Just WordCharY
  'z' -> Just WordCharZ
  _ -> Nothing
