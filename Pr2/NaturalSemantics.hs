{-|

Programming Languages
Fall 2023

Implementation of the Natural Semantics of the WHILE Language

Author: Isidro Javier Garcia Fernandez

-}

module NaturalSemantics where

import           Aexp
import           Bexp
import           State
import           While

-- representation of configurations for WHILE

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

------------------------------------------

data Update = Var :=>: Z

update :: State -> Update -> State
update s (y:=>:v) x 
    | x == y = v
    | otherwise = s x

-- representation of the execution judgement <S, s> -> s'

nsStm :: Config -> Config


nsStm (Inter (Ass x a) s)      = Final (update s (x:=>: aVal a s))

-- skip

nsStm (Inter Skip s)           = Final s

-- s1; s2

nsStm (Inter (Comp ss1 ss2) s) = Final s''
  where
    Final s' = nsStm (Inter ss1 s)
    Final s'' = nsStm (Inter ss2 s')

-- if b then s1 else s2

-- B[b]s = tt
nsStm (Inter (If b ss1 ss2) s)
  | bVal b s == True = Final s'
  where
    Final s' = nsStm (Inter ss1 s)

-- B[b]s = ff
nsStm (Inter (If b ss1 ss2) s)
  | bVal b s == False = Final s'
  where
    Final s' = nsStm (Inter ss2 s)

-- while b do s

-- B[b]s = ff
nsStm (Inter (While b ss) s)
  | bVal b s == False = Final s

-- B[b]s = tt
nsStm (Inter (While b ss) s)
  | bVal b s == True = Final s''
      where 
        Final s' = nsStm (Inter ss s)
        Final s'' = nsStm(Inter (While b ss) s')

-- repeat S until b
-- (Repeat Stm Bexp)
--B[b] s = tt
nsStm (Inter (Repeat ss b) s)
  | bVal b s == True = Final s'
    where 
       Final s' = nsStm (Inter ss s)

--B[b] s = ff
nsStm (Inter (Repeat ss b) s)
  | bVal b s == False = Final s''
    where 
      Final s' = nsStm (Inter ss s)
      Final s'' = nsStm (Inter (Repeat ss b) s')

-- for x := a1 to a2 do S
-- (For Var Aexp Aexp Stm)
-- B[a1 <= a2] s = ff
nsStm (Inter (For x a1 a2 ss) s)
  | bVal (Leq a1 a2) s  == False = Final s  -- Si a1 > a2, no se ejecuta el for, se devuelve el estado inicial

-- B[a1 <= a2] s = tt
nsStm (Inter (For x a1 a2 ss) s0)
  | bVal (Leq a1 a2) s0 = Final s3
    where
      Final s1 = nsStm (Inter (Ass x a1) s0)
      Final s2 = nsStm (Inter ss s1)
      Final s3 = nsStm (Inter (For x (Add v1 (N "1")) v2 ss) s2)
      v1 = N (show (aVal a1 s0))
      v2 = N (show (aVal a2 s0))

-- semantic function for natural semantics
sNs :: Stm -> State -> State
sNs ss s = s'
  where Final s' = nsStm (Inter ss s)
