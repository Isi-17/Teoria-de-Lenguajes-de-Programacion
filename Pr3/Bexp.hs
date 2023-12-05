-- Bexp.hs - implementation of Bexp

-- -------------------------------------------------------------------
-- Abstract syntax of Boolean expressions
-- -------------------------------------------------------------------

module Bexp where

import           Aexp
import           State

data  Bexp  =  TRUE
            |  FALSE
            |  Equ Aexp Aexp
            |  Leq Aexp Aexp
            |  Neg Bexp
            |  And Bexp Bexp
            deriving (Show, Eq)

---------------------------------------------------------------------
-- Semantics of Boolean expressions
---------------------------------------------------------------------

type  T      =  Bool

bVal :: Bexp -> State -> T
bVal TRUE _        =  True
bVal FALSE _       =  False
bVal (Equ a1 a2) s =  aVal a1 s == aVal a2 s
bVal (Leq a1 a2) s =  aVal a1 s <= aVal a2 s
bVal (Neg b) s     =  not (bVal b s)
bVal (And b1 b2) s =  bVal b1 s && bVal b2 s
