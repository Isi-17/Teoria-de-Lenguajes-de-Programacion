-- -------------------------------------------------------------------
-- While21.hs
-- -------------------------------------------------------------------

module While21 where

type  Var   =  String
type  NumLit = String

data  Aexp  =  N NumLit
            |  V Var
            |  Add Aexp Aexp
            |  Mult Aexp Aexp
            |  Sub Aexp Aexp
            |  Div Aexp Aexp
            deriving (Show, Eq)

data  Bexp  =  TRUE
            |  FALSE
            |  Eq Aexp Aexp
            |  Le Aexp Aexp
            |  Neg Bexp
            |  And Bexp Bexp
            deriving (Show, Eq)

data  Stm   =  Ass Var Aexp
            |  Skip
            |  Comp Stm Stm
            |  If Bexp Stm Stm
            |  Case Aexp LabelledStms        -- todo
            |  Abort
            deriving Show

data LabelledStms = LabelledStm [Integer] Stm LabelledStms
                  | Default Stm
                  | EndLabelledStms
                  deriving Show

type  Z      =  Integer
type  T      =  Bool
type  State  =  Var -> Z


numLit :: NumLit -> Z
numLit = read

aVal :: Aexp -> State -> Z
aVal (N n) _        =  numLit n
aVal (V x) s        =  s x
aVal (Add a1 a2) s  =  aVal a1 s + aVal a2 s
aVal (Sub a1 a2) s  =  aVal a1 s - aVal a2 s
aVal (Mult a1 a2) s =  aVal a1 s * aVal a2 s
aVal (Div a1 a2) s  =  aVal a1 s `div` aVal a2 s

bVal :: Bexp -> State -> T
bVal TRUE _        =  True
bVal FALSE _       =  False
bVal (Eq a1 a2) s  =  aVal a1 s == aVal a2 s
bVal (Le a1 a2) s  =  aVal a1 s <= aVal a2 s
bVal (Neg b) s     =  not(bVal b s)
bVal (And b1 b2) s =  bVal b1 s && bVal b2 s
