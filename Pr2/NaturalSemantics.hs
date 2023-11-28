{-|

Programming Languages
Fall 2023

Implementation of the Natural Semantics of the WHILE Language

Author:

-}

module NaturalSemantics where

import           Aexp
import           Bexp
import           State
import           While

-- representation of configurations for WHILE

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

-- representation of the execution judgement <S, s> -> s'

nsStm :: Config -> Config

-- x := a

nsStm (Inter (Ass x a) s)      = undefined

-- skip

nsStm (Inter Skip s)           = undefined

-- s1; s2

nsStm (Inter (Comp ss1 ss2) s) = undefined

-- if b then s1 else s2

-- B[b]s = tt
nsStm (Inter (If b ss1 ss2) s) = undefined

-- B[b]s = ff
nsStm (Inter (If b ss1 ss2) s) = undefined

-- while b do s

-- B[b]s = ff
nsStm (Inter (While b ss) s)   = undefined

-- B[b]s = tt
nsStm (Inter (While b ss) s)   = undefined

-- semantic function for natural semantics
sNs :: Stm -> State -> State
sNs ss s = s'
  where Final s' = nsStm (Inter ss s)
