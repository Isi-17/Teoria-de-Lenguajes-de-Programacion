-- Aexp
-- a ::= x | x | a1 + a2 | a1 * a2 | a1 - a2
-- La categoría n vendría definida por Num
-- La cateogría de x vendría definida por Var
module Aexp where

type Var = String
type Z = Integer
type State = Var -> Z   -- no choca 'Var' porque depende del contexto

data Aexp = Lit String   -- Mejor que poner Num String, pues Num ya se utiliza
        |   VarId Var         -- hemos puesto 'Var' en lugar de 'String' para que se entienda mejor
        |   Add Aexp Aexp   -- la suma usa dos expresiones tipo Aexp
        |   Prod Aexp Aexp  -- el producto usa dos expresiones tipo Aexp
        |   Sub Aexp Aexp   -- la resta usa dos expresiones tipo Aexp0
          deriving Show
        --deriving (Show, Eq)

-- exp0 = (x + 3) * (y - 5)
--              *
--         +        -
--      x    3    y   5
exp0 :: Aexp
exp0 = Prod (Add (VarId "x") (Lit "3")) (Sub (VarId "y") (Lit "5"))   -- los parentesis son de la sintaxis de Haskell, no de la sintaxis abstracta creada

-- A : Aexp -> State -> Z
-- A[n] = N[n]
-- A[x]s = sx
-- A[a1 + a2] = A[a1]s + A[a2]s
-- A[a1 * a2] = A[a1]s * A[a2]s
-- A[a1 - a2] = A[a1]s - A[a2]s

evalAexp :: Aexp -> State -> Z
evalAexp (Lit n) s = read n     -- read es una función de Haskell que convierte un String en un Integer
evalAexp (VarId x) s = s x
evalAexp (Add a1 a2) s = evalAexp a1 s + evalAexp a2 s
evalAexp (Prod a1 a2) s = evalAexp a1 s * evalAexp a2 s
evalAexp (Sub a1 a2) s = evalAexp a1 s - evalAexp a2 s


-- read "3" :: Int  -- esto es lo que hace la función read, devuelve un Integer '3'

-- Vamos a definir estados
s0 :: State
s0 "x" = 2
s0 "y" = 7
s0 "z" = 15
s0 "media" = 45
s0 _ = 0                -- para que la funcion sea total

-- evalAexp exp0 s0    devuelve 10

-- x := 5
-- y := x++
-- Se tiene que x = 6, y = 5

type T = Bool

data  Bexp  =  TRUE 
            |  FALSE 
            |  Eq Aexp Aexp 
            |  Le Aexp Aexp 
            |  Neg Bexp 
            |  And Bexp Bexp 
              deriving Show
            --deriving (Show, Eq) 

evalBexp :: Bexp -> State -> T
evalBexp TRUE _  = True
evalBexp FALSE _ = False
evalBexp (Eq a1 a2) s  =  evalAexp a1 s == evalAexp a2 s
evalBexp (Le a1 a2) s  =  evalAexp a1 s <= evalAexp a2 s 
evalBexp (Neg b) s  =  not(evalBexp b s)
evalBexp (And b1 b2) s  =  evalBexp b1 s && evalBexp b2 s