
module Binario  where
-- Binary Numerals
-- N : Num -> Z is defined by the semantic clauses or equations:
-- N [[0]] = 0
-- N [[1]] = 1
-- N [[n 0]] = 2 · N [[n]]
-- N [[n 1]] = 2 · N [[n]] + 1

data Binary = O         -- 0
            | I         -- 1
            | SO Binary     -- binario que acaba en 0
            | SI Binary     -- binario que acaba en 1
            deriving Show

-- 1011
b0 :: Binary
b0 = SI( SI (SO I))
-- acabo en 1, acabo en 0, acabo en 1, acabo en 0. Leemos de fin a principio.

-- Evaluate binary numbers
evalBin :: Binary -> Integer
evalBin O = 0
evalBin I = 1
evalBin (SO b) = 2 * evalBin b
evalBin (SI b) = 2 * evalBin b + 1

--evalBin b0 = 11


-- N : Num -> Z is defined by the semantic clauses or equations:
-- N [[0]] = 0
-- N [[1]] = 1
-- N [[0 n]] = N [[n]]    -- los 0 a la izquierda no tienen valor
-- N [[1 n]] =            -- no se puede construir el significado a partir de los componentes
-- Esto ocurre por la sintaxis.

-- Si n es un numero binario que empieza por 1, entonces 


