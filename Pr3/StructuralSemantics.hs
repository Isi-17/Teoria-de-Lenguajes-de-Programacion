{-|

Programming Languages
Fall 2023

Implementation of the Structural Operational Semantics of the WHILE Language

Author:

-}

module StructuralSemantics where

import           Aexp
import           Bexp
import           State
import           While

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s
            | Stuck Stm State  -- <S, s>

isFinal :: Config -> Bool
isFinal (Final _) = True
isFinal _         = False

isInter :: Config -> Bool
isInter (Inter _ _) = True
isInter _           = False

isStuck :: Config -> Bool
isStuck (Stuck _ _) = True
isStuck _           = False

-- representation of the transition relation <S, s> => gamma

sosStm :: Config -> Config

-- x := a

sosStm = undefined

-- skip

-- todo

-- s1; s2

-- todo

-- if b then s1 else s2

-- todo

-- while b do s

-- todo

-- repeat s until b

-- todo

-- for x a1 to a2 s

-- todo

-- abort

-- todo
