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
data Update = Var :=>: Z

update :: State -> Update -> State
update s (x :=>: v) y 
    | x == y = v
    | otherwise = s y

sosStm :: Config -> Config

-- x := a

sosStm (Inter (Ass x a) s) = Final (update s (x :=>: (aVal a s)))

-- skip

sosStm (Inter Skip s) = Final s

-- s1; s2
---- comp1
sosStm (Inter (Comp ss1 ss2) s)
    | isInter next = Inter (Comp ss1' ss2) s'
    where 
        next = sosStm (Inter ss1 s)
        Inter ss1' s' = next

---- comp2
sosStm (Inter (Comp ss1 ss2) s)
    | isFinal next = Inter ss2 s'
    where 
        next = sosStm (Inter ss1 s)
        Final s' = next

-- if b then s1 else s2

-- B[b]s = tt
sosStm (Inter (If b ss1 ss2) s)
    | bVal b s == True = Inter ss1 s

-- B[b]s = ff
sosStm (Inter (If b ss1 ss2) s)
    | bVal b s == False = Inter ss2 s
    
-- while b do s

sosStm (Inter (While b ss1) s) = Inter (If b (Comp ss1 (While b ss1)) Skip) s

-- repeat s until b

-- sosStm (Inter (Repeat ss1 b) s) = Inter (If b ss1 (Comp ss1 (Repeat ss1 b))) s
sosStm (Inter (Repeat ss b) s) = Inter (Comp ss (If b Skip (Repeat ss b))) s

-- for x a1 to a2 s

sosStm (Inter (For x a1 a2 ss) s) = Inter (If (Leq a1 a2) (Comp (Ass x a1) (For x (Add v1 (N "1")) v2 ss)) Skip ) s
    where 
        v1 = N ( show (aVal a1 s))
        v2 = N ( show (aVal a2 s))

-- abort

sosStm (Inter Abort s) = Stuck Abort s

-- Define assert in StructuralSemantics
--
--            -------------------------------------------------------
--             <assert b before S, s> => <if b then S else abort, s>
--
-- sosStm (Inter (Assert b S) s) = Inter (If b S Skip) s
--
-- Si no tenemos definido abort:
--
--            -----------------------------------------------------
--             <assert b before S, s> => <if b then S else skip, s> 
--
-- sosStm (Inter (Assert b S) s) = Inter (If b S Abort) s

