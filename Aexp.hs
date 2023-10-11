-- Aexp
-- a ::= x | x | a1 + a2 | a1 * a2 | a1 - a2
-- La categoría n vendría definida por Num
-- La cateogría de x vendría definida por Var
module Aexp where

data Aexp = Lit String   -- Mejor que poner Num String, pues Num ya se utiliza
        |   Var String
        |   Add Aexp Aexp   -- la suma usa dos expresiones tipo Aexp
        |   Prod Aexp Aexp  -- el producto usa dos expresiones tipo Aexp
        |   Sub Aexp Aexp   -- la resta usa dos expresiones tipo Aexp
        deriving Show

-- exp0 = (x + 3) * (y - 5)
--              *
--         +        -
--      x    3    y   5
exp0 :: Aexp
exp0 = Prod (Add (Var "x") (Lit "3")) (Sub (Var "y") (Lit "5"))   -- los parentesis son de la sintaxis de Haskell, no de la sintaxis abstracta creada

