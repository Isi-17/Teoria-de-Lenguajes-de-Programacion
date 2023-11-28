-- Aexp.hs - Implementation of Aexp

-- -------------------------------------------------------------------
-- Abstract syntax of arithmetic expressions
-- -------------------------------------------------------------------

module Aexp where

import           State

type  NumLit = String

data  Aexp  =  N NumLit
            |  V Var
            |  Add Aexp Aexp
            |  Mult Aexp Aexp
            |  Sub Aexp Aexp
            deriving (Show, Eq)

---------------------------------------------------------------------
-- Semantics of arithmetic expressions
---------------------------------------------------------------------

numLit :: NumLit -> Z
numLit = read

aVal :: Aexp -> State -> Z
aVal (N n) _        =  numLit n
aVal (V x) s        =  s x
aVal (Add a1 a2) s  =  aVal a1 s + aVal a2 s
aVal (Mult a1 a2) s =  aVal a1 s * aVal a2 s
aVal (Sub a1 a2) s  =  aVal a1 s - aVal a2 s
