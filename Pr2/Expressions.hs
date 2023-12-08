{-

Programming Languages
Fall 2023

Semantics of Expressions

-}

module Expressions where

import           Aexp
import           Bexp
import           State


import           Test.HUnit hiding (State)

-- |----------------------------------------------------------------------
-- | Exercise 1 - Semantics of binary numerals
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Bin' for the binary numerals:

data Bit = O
         | I
         deriving (Eq, Show)

data Bin = MSB Bit
         | B Bin Bit
         deriving (Eq, Show)

-- | and the following values of type 'Bin':

zero :: Bin
zero = MSB O

one :: Bin
one = MSB I

three :: Bin
three = B (B (MSB O) I) I

six :: Bin
six = B (B (MSB I) I) O

-- | define a semantic function 'binVal' that associates
-- | a number (in the decimal system) to each binary numeral.

-- binVal :: Bin -> Z  -- N : Num -> Z
-- binVal (MSB bit) 
--     | bit == O = 0 
--     | bit == I = 1  
-- binVal (B bin bit) 
--   | bit == O = 2*(binVal bin)       --   2 * N[n]
--   | otherwise = 2*(binVal bin) + 1  --   2 * N[n] + 1

binVal :: Bin -> Z
binVal (MSB bit) = bitVal bit -- MSB :: Bit -> Bin ----> bitVal :: Bit -> Z
binVal (B bin bit) = 2*(binVal bin) + bitVal bit

bitVal :: Bit -> Z
bitVal O = 0
bitVal I = 1
 

-- | Test your function with HUnit.

testBinVal :: Test
testBinVal = test ["value of zero"  ~: 0 ~=? binVal zero,
                   "value of one"   ~: 1 ~=? binVal one,
                   "value of three" ~: 3 ~=? binVal three,
                   "value of six"   ~: 6 ~=? binVal six]

-- | Define a function 'foldBin' to fold a value of type 'Bin'

foldBin :: (a -> Bit -> a) -> (Bit -> a) -> Bin -> a
foldBin b msb bin = plegar bin
    where 
      plegar (MSB bit) = msb bit -- MSB :: Bit -> Bin ----> bitVal :: Bit -> Z
      plegar (B bin bit) = b (plegar bin) bit -- B :: Bin -> Bit -> Bin ----> b :: a -> Bit -> a


-- | and use 'foldBin' to define a function 'binVal''  equivalent to 'binVal'.

binVal' :: Bin -> Integer
binVal' bin = foldBin (\n bit -> 2*n + bitVal bit) bitVal bin

-- | Test your function with HUnit.

-- todo

-- | Define a function 'normalize' that given a binary numeral trims leading zeroes.
-- | For example, normalize (B (B (MSB O) I) I) should return (B (MSB I) I).
-- normalize :: Bin -> Bin
-- normalize (MSB bit) = (MSB bit) -- normalize msb@(MSB _) = msb
-- normalize (B (MSB O) bit) = (MSB bit)
-- normalize (B bin bit) = (B (normalize bin) bit)
normalize :: Bin -> Bin
normalize msb@(MSB _) = msb -- msb = MSB bit
normalize (B bin bit) = addBit (normalize bin) bit

addBit :: Bin -> Bit -> Bin
addBit (MSB O) bit2 = MSB bit2 -- al normalizar da 0 lo quitamos
addBit bin bit = B bin bit -- esto incluye lo de abajo
-- addBit (MSB I) bit2 = B (MSB I) bit2
-- addBit (B bin bit) bit2 = B (B bin bit) bit2

-- | Define a function 'hammingWeight' that returns the number of ones occurring
-- | in a binary numeral.

hammingWeight :: Bin -> Integer
hammingWeight (MSB O) = 0
hammingWeight (MSB I) = 1
hammingWeight (B b O) = hammingWeight b
hammingWeight (B b I) = 1 + hammingWeight b

-- | and use 'foldBin' to define a function 'hammingWeight''  equivalent to 'hammingWeight'.

hammingWeight' :: Bin -> Integer
hammingWeight' = undefined
-- hammingWeight' b = foldBin (\x -> if x == O then 0 else 1) (+) b

-- | Test your functions with HUnit.

testHammingWeight :: Test
testHammingWeight = test ["value of zero"  ~: 0 ~=? hammingWeight zero,
                   "value of one"   ~: 1 ~=? hammingWeight one,
                   "value of three" ~: 2 ~=? hammingWeight three,
                   "value of six"   ~: 2 ~=? hammingWeight six,
                   "value of zero'"  ~: 0 ~=? hammingWeight' zero,
                   "value of one'"   ~: 1 ~=? hammingWeight' one,
                   "value of three'" ~: 2 ~=? hammingWeight' three,
                   "value of six'"   ~: 2 ~=? hammingWeight' six]

-- | Define a function 'complement' that returns the complement of a binary numeral

complement :: Bin -> Bin
complement (MSB O) = MSB I
complement (MSB I) = MSB O
complement (B b O) = B (complement b) I
complement (B b I) = B (complement b) O

-- | and use 'foldBin' to define a function 'complement''  equivalent to 'complement'.

complement' :: Bin -> Bin
complement' = undefined
-- complement' b = foldBin (\x -> if x == O then MSB I else MSB O) (\(MSB x) acc -> B acc x) b

-- | Test your functions with HUnit.

testComplement :: Test
testComplement = test ["value of zero"  ~: (MSB I) ~=? complement zero,
                   "value of one"   ~: (MSB O) ~=? complement one,
                   "value of three" ~: (B (B (MSB I) O) O) ~=? complement three,
                   "value of six"   ~: (B (B (MSB O) O) I) ~=? complement six,
                   "value of zero'"  ~: (MSB I) ~=? complement' zero,
                   "value of one'"   ~: (MSB O) ~=? complement' one,
                   "value of three'" ~: (B (B (MSB I) O) O) ~=? complement' three,
                   "value of six'"   ~: (B (B (MSB O) O) I) ~=? complement' six]


-- | and use 'foldBin' to define a function 'normalize''  equivalent to 'normalize'.

normalize' :: Bin -> Bin
normalize' bin = foldBin addBit MSB bin -- foldBin :: (a -> Bit -> a) -> (Bit -> a) -> Bin -> a


-- | Test your functions with HUnit.

testnormalize :: Test 
testnormalize = test ["First Test" ~: B (B (MSB I) O) I ~=? normalize (B (B (B (B (MSB O) O) I) O) I), 
                      "Second Test" ~: MSB O ~=? normalize (B (B (B (B (MSB O) O) O) O) O)]

testnormalize' :: Test
testnormalize' = test ["First Test" ~: B (B (MSB I) O) I ~=? normalize' (B (B (B (B (MSB O) O) I) O) I), 
                       "Second Test" ~: MSB O ~=? normalize' (B (B (B (B (MSB O) O) O) O) O)] 


-- |----------------------------------------------------------------------
-- | Exercise 2 - Free variables of expressions
-- |----------------------------------------------------------------------
-- | Define the function 'fvAexp' that computes the set of free variables
-- | occurring in an arithmetic expression. Ensure that each free variable
-- | occurs once in the resulting list.
-- Variables de la Aexp eliminando elementos repetidos
fvAexp :: Aexp -> [Var]
fvAexp (N n) = []
fvAexp (V x) = [x]
fvAexp (Add a1 a2) = eliminarOcurrencia (fvAexp a1 ++ fvAexp a2) -- eliminar repetidos en concatenacion
fvAexp (Mult a1 a2) = eliminarOcurrencia (fvAexp a1 ++ fvAexp a2)
fvAexp (Sub a1 a2) = eliminarOcurrencia (fvAexp a1 ++ fvAexp a2)

eliminarOcurrencia :: [Var] -> [Var]
eliminarOcurrencia [] = []
eliminarOcurrencia (x:xs) = if elem x xs   
                              then eliminarOcurrencia (xs)
                              else x:(eliminarOcurrencia xs)


-- | Test your function with HUnit.

testfvAexp :: Test
testfvAexp = test ["FirstTest" ~: ["x"] ~=? fvAexp(Add (V "x") (V "x")),
                   "SecondTest" ~: ["x", "y"] ~=? fvAexp(Add (V "x") (V "y")),
                   "ThirdTest" ~: ["x", "y"] ~=? fvAexp((Mult (Sub (V "x") (V "y"))) (Add (V "y") (V "x")))]

-- | Define the function 'fvBexp' that computes the set of free variables
-- | occurring in a Boolean expression.

-- Variables de Bexp eliminando elementos repetidos
fvBexp :: Bexp -> [Var]
fvBexp (TRUE) = [] -- no variables
fvBexp (FALSE) = [] -- no variables
fvBexp (Equ a1 a2) = eliminarOcurrencia (fvAexp a1 ++ fvAexp a2) -- Concatena Aexp Aexp
fvBexp (Leq a1 a2) = eliminarOcurrencia (fvAexp a1 ++ fvAexp a2) -- Concatena Aexp Aexp
fvBexp (Neg b) = eliminarOcurrencia (fvBexp b)  -- Bexp
fvBexp (And b1 b2) = eliminarOcurrencia(fvBexp b1 ++ fvBexp b2) -- Concatena Bexp Bexp

-- | Test your function with HUnit.

testfvBexp :: Test
testfvBexp = test ["FirstTest" ~: ["x"] ~=? fvBexp(Equ (V "x") (V "x")),
                   "SecondTest" ~: ["x", "y"] ~=? fvBexp(Leq (V "x") (V "y")),
                   "ThirdTest" ~: ["x", "y"] ~=? fvBexp(Leq  (Sub (V "x") (V "y")) (Add (V "y") (V "x"))),
                   "FourthTest" ~: ["y"] ~=? fvBexp(Neg (Equ (V "y") (V "y")))]

-- |----------------------------------------------------------------------
-- | Exercise 3 - Substitution of variables in expressions
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Subst' for representing substitutions:

data Subst = Var :->: Aexp

-- | define a function 'substAexp' that takes an arithmetic expression
-- | 'a' and a substitution 'y:->:a0' and returns the substitution a [y:->:a0];
-- | i.e., replaces every occurrence of 'y' in 'a' by 'a0'.

substAexp :: Aexp -> Subst -> Aexp
substAexp (N n) (y :->: a0) = N n
substAexp (V x) (y :->: a0) = if x == y then a0 else V x
substAexp (Add a1 a2) (y :->: a0) = Add (substAexp a1 (y :->: a0)) (substAexp a2 (y :->: a0))
substAexp (Mult a1 a2) (y :->: a0) = Mult (substAexp a1 (y :->: a0)) (substAexp a2 (y :->: a0))
substAexp (Sub a1 a2) (y :->: a0) = Sub (substAexp a1 (y :->: a0)) (substAexp a2 (y :->: a0))

-- | Test your function with HUnit.

testsubstAexp :: Test
testsubstAexp = test ["FirstTest" ~: (Add (V "x") (V "x")) ~=? substAexp (Add (V "x") (V "y")) ("y" :->: (V "x")),
                      "SecondTest" ~: (Add (V "x") (V "x")) ~=? substAexp (Add (V "x") (V "x")) ("y" :->: (V "2")),
                      "ThirdTest" ~: (Mult (V "3") (V "y")) ~=? substAexp (Mult (V "x") (V "y")) ("x" :->: (V "3")),
                      "FourthTest" ~: (Sub (V "y") (V "y")) ~=? substAexp (Sub (V "x") (V "x")) ("x" :->: (V "y"))]

-- | Define a function 'substBexp' that implements substitution for
-- | Boolean expressions.

substBexp :: Bexp -> Subst -> Bexp
substBexp (TRUE) (y :->: a0) = TRUE
substBexp (FALSE) (y :->: a0) = FALSE
substBexp (Equ a1 a2) (y :->: a0) = Equ (substAexp a1 (y :->: a0)) (substAexp a2 (y :->: a0))
substBexp (Leq a1 a2) (y :->: a0) = Leq (substAexp a1 (y :->: a0)) (substAexp a2 (y :->: a0))
substBexp (Neg b) (y :->: a0) = Neg (substBexp b (y :->: a0))
substBexp (And b1 b2) (y :->: a0) = And (substBexp b1 (y :->: a0)) (substBexp b2 (y :->: a0))

-- | Test your function with HUnit.

testsubstBexp :: Test
testsubstBexp = test ["FirstTest" ~: (Equ (V "x") (V "x")) ~=? substBexp (Equ (V "x") (V "y")) ("y" :->: (V "x")),
                      "SecondTest" ~: (Equ (V "x") (V "x")) ~=? substBexp (Equ (V "x") (V "x")) ("y" :->: (V "2")),
                      "ThirdTest" ~: (Leq (V "3") (V "y")) ~=? substBexp (Leq (V "x") (V "y")) ("x" :->: (V "3")),
                      "FourthTest" ~: (Neg (Equ (V "y") (V "y"))) ~=? substBexp (Neg (Equ (V "x") (V "x"))) ("x" :->: (V "y")),
                      "FifthTest" ~: (And (Equ (V "x") (V "x")) (Leq (V "x") (V "x"))) ~=? substBexp (And (Equ (V "x") (V "y")) (Leq (V "x") (V "y"))) ("y" :->: (V "x"))]
-- |----------------------------------------------------------------------
-- | Exercise 4 - Update of state
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Update' for state updates:

data Update = Var :=>: Z

-- | define a function 'update' that takes a state 's' and an update 'x :=> v'
-- | and returns the updated state 's [x :=> v]'

update :: State -> Update -> State
update s (y:=>:v) x 
    | x == y = v
    | otherwise = s x

-- | Test your function with HUnit.

testupdate :: Test
testupdate = test ["FirstTest" ~: 1 ~=? (update (\y -> 0) ("x" :=>: 1)) "x",
                   "SecondTest" ~: 0 ~=? (update (\y -> 0) ("x" :=>: 1)) "y",
                   "ThirdTest" ~: 1 ~=? (update (\y -> 0) ("x" :=>: 1)) "x"]

-- | Define a function 'updates' that takes a state 's' and a list of updates
-- | 'us' and returns the updated states resulting from applying the updates
-- | in 'us' from head to tail. For example:
-- |
-- |    updates s ["x" :=>: 1, "y" :=>: 2, "x" :=>: 3]
-- |
-- | returns a state that binds "x" to 3 (the most recent update for "x").

updates :: State ->  [Update] -> State
updates s [] = s -- si no hay updates, regresa el estado
updates s (u:us) = updates (update s u) us -- si hay updates, actualiza el estado y sigue con los demas updates


-- | Test your function with HUnit.

testupdates :: Test
testupdates = test ["FirstTest" ~: 1 ~=? (updates (\y -> 0) [("x" :=>: 1), ("y" :=>: 2), ("x" :=>: 3)]) "x",
                    "SecondTest" ~: 2 ~=? (updates (\y -> 0) [("x" :=>: 1), ("y" :=>: 2), ("x" :=>: 3)]) "y",
                    "ThirdTest" ~: 0 ~=? (updates (\y -> 0) [("x" :=>: 1), ("y" :=>: 2), ("x" :=>: 3)]) "z"]
-- comando para probar el test: runTestTT testupdates

-- |----------------------------------------------------------------------
-- | Exercise 5 - Folding expressions
-- |----------------------------------------------------------------------
-- | Define a function 'foldAexp' to fold an arithmetic expression

foldAexp :: (NumLit -> a) -> (Var -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Aexp -> a
foldAexp flit fvar fadd fmult fsub x = plegar x
    where
      plegar (N n) = flit n
      plegar (V x) = fvar x
      plegar (Add a1 a2) = fadd (plegar a1) (plegar a2)
      plegar (Mult a1 a2) = fmult (plegar a1) (plegar a2)
      plegar (Sub a1 a2) = fsub (plegar a1) (plegar a2)


-- | Use 'foldAexp' to define the functions 'aVal'', 'fvAexp'', and 'substAexp''.
-- Obtener el valor de una expresion aritmetica
aVal' :: Aexp -> State -> Z
aVal' exp s = foldAexp read s (+) (*) (-) exp

-- aVal :: Aexp -> State -> Z
-- aVal (N n) _        =  numLit n -- read :: Read a => String -> a
-- aVal (V x) s        =  s x
-- aVal (Add a1 a2) s  =  aVal a1 s + aVal a2 s
-- aVal (Mult a1 a2) s =  aVal a1 s * aVal a2 s
-- aVal (Sub a1 a2) s  =  aVal a1 s - aVal a2 s

-- obtener las variables de una expresion aritmetica
-- No se pueden repetir las variables
fvAexp' :: Aexp -> [Var]
fvAexp' exp = purgar (foldAexp (\n -> []) (\x -> [x]) (++) (++) (++) exp)
      where purgar [] = []
            purgar (x:xs) = if elem x xs   
                              then purgar (xs)
                              else x:(purgar xs)

-- Si es N, no hay variables
-- Si es V, hay una variable
-- Si es Add, Mult, Sub, hay variables en a1 y a2

-- Sustituir una variable por una expresion aritmetica
substAexp' :: Aexp -> Subst -> Aexp
substAexp' exp (y :->: a0) = foldAexp N (\x -> if x == y then a0 else V x) Add Mult Sub exp

-- | Test your functions with HUnit.

testaVal' :: Test
testaVal' = test ["FirstTest" ~: 2 ~=? aVal' (Add (N "1") (N "1")) (\y -> 0),
                  "SecondTest" ~: 0 ~=? aVal' (Add (N "1") (N "1")) (\y -> 1), -- 1 + 1 = 2 != 0
                  "ThirdTest" ~: 1 ~=? aVal' (Mult (N "1") (N "1")) (\y -> 1),
                  "FourthTest" ~: 0 ~=? aVal' (Sub (N "1") (N "1")) (\y -> 1)]

testfvAexp' :: Test
testfvAexp' = test ["FirstTest" ~: ["x"] ~=? fvAexp' (Add (V "x") (V "x")),
                   "SecondTest" ~: ["x", "y"] ~=? fvAexp' (Add (V "x") (V "y")),
                   "ThirdTest" ~: ["y", "x"] ~=? fvAexp' (Mult (Sub (V "x") (V "y")) (Add (V "y") (V "x")))]

testsubstAexp' :: Test
testsubstAexp' = test ["FirstTest" ~: (Add (V "x") (V "x")) ~=? substAexp' (Add (V "x") (V "y")) ("y" :->: (V "x")),
                      "SecondTest" ~: (Add (V "x") (V "x")) ~=? substAexp' (Add (V "x") (V "x")) ("y" :->: (V "2")),
                      "ThirdTest" ~: (Mult (V "3") (V "y")) ~=? substAexp' (Mult (V "x") (V "y")) ("x" :->: (V "3")),
                      "FourthTest" ~: (Sub (V "y") (V "y")) ~=? substAexp' (Sub (V "x") (V "x")) ("x" :->: (V "y"))]

-- | Define a function 'foldBexp' to fold a Boolean expression and use it
-- | to define the functions 'bVal'', 'fvBexp'', and 'substAexp''.

foldBexp :: a -> a -> (Aexp -> Aexp -> a) -> (Aexp -> Aexp -> a) -> (a -> a) -> (a -> a -> a) -> Bexp -> a
foldBexp fT fF fEqu fLeg fNeg fAnd x = plegar x
      where 
        plegar TRUE = fT
        plegar FALSE = fF
        plegar (Equ a1 a2) = fEqu a1 a2
        plegar (Leq a1 a2) = fLeg a1 a2
        plegar (Neg b) = fNeg (plegar b)
        plegar (And b1 b2) = fAnd (plegar b1) (plegar b2)

-- | Test your functions with HUnit.
-- bVal obtiene el valor de una expresion booleana
bVal' :: Bexp -> State -> Bool
bVal' exp s = foldBexp True False (\a1 a2 -> aVal' a1 s == aVal' a2 s) (\a1 a2 -> aVal' a1 s <= aVal' a2 s) (not) (&&) exp

-- fvBexp obtiene las variables de una expresion booleana
fvBexp' :: Bexp -> [Var]
fvBexp' exp = purgar (foldBexp [] [] (\a1 a2 -> fvAexp' a1 ++ fvAexp' a2) (\a1 a2 -> fvAexp' a1 ++ fvAexp' a2) id (++) exp)
      where purgar [] = []
            purgar (x:xs) = if elem x xs   
                              then purgar (xs)
                              else x:(purgar xs)

-- substBexp sustituye una variable por una expresion aritmetica
substBexp' :: Bexp -> Subst -> Bexp
substBexp' exp sus = foldBexp TRUE FALSE subEsq subLeq (Neg) (And) exp
      where subEsq a1 a2 = Equ (substAexp' a1 sus) (substAexp' a2 sus)
            subLeq a1 a2 = Leq (substAexp' a1 sus) (substAexp' a2 sus)

-- | Test your functions with HUnit.

testbVal' :: Test
testbVal' = test ["FirstTest" ~: True ~=? bVal' (Equ (N "1") (N "1")) (\y -> 0),
                  "SecondTest" ~: False ~=? bVal' (Equ (N "1") (N "1")) (\y -> 1), -- 1 == 1 = True != False
                  "ThirdTest" ~: True ~=? bVal' (Leq (N "1") (N "1")) (\y -> 1),
                  "FourthTest" ~: False ~=? bVal' (Leq (N "1") (N "1")) (\y -> 0),
                  "FifthTest" ~: True ~=? bVal' (Neg (Equ (N "1") (N "1"))) (\y -> 0),
                  "SixthTest" ~: False ~=? bVal' (Neg (Equ (N "1") (N "1"))) (\y -> 1),
                  "SeventhTest" ~: True ~=? bVal' (And (Equ (N "1") (N "1")) (Leq (N "1") (N "1"))) (\y -> 1),
                  "EighthTest" ~: False ~=? bVal' (And (Equ (N "1") (N "1")) (Leq (N "1") (N "1"))) (\y -> 0)]

testfvBexp' :: Test
testfvBexp' = test ["FirstTest" ~: ["x"] ~=? fvBexp' (Equ (V "x") (V "x")),
                   "SecondTest" ~: ["x", "y"] ~=? fvBexp' (Leq (V "x") (V "y")),
                   "ThirdTest" ~: ["y", "x"] ~=? fvBexp' (Leq  (Sub (V "x") (V "y")) (Add (V "y") (V "x"))),
                   "FourthTest" ~: ["y"] ~=? fvBexp' (Neg (Equ (V "y") (V "y"))),
                   "FifthTest" ~: ["x"] ~=? fvBexp' (And (Equ (V "x") (V "x")) (Leq (V "x") (V "x")))]

testsubstBexp' :: Test
testsubstBexp' = test ["FirstTest" ~: (Equ (V "x") (V "x")) ~=? substBexp' (Equ (V "x") (V "y")) ("y" :->: (V "x")),
                      "SecondTest" ~: (Equ (V "x") (V "x")) ~=? substBexp' (Equ (V "x") (V "x")) ("y" :->: (V "2")),
                      "ThirdTest" ~: (Leq (V "3") (V "y")) ~=? substBexp' (Leq (V "x") (V "y")) ("x" :->: (V "3")),
                      "FourthTest" ~: (Neg (Equ (V "y") (V "y"))) ~=? substBexp' (Neg (Equ (V "x") (V "x"))) ("x" :->: (V "y")),
                      "FifthTest" ~: (And (Equ (V "x") (V "x")) (Leq (V "x") (V "x"))) ~=? substBexp' (And (Equ (V "x") (V "y")) (Leq (V "x") (V "y"))) ("y" :->: (V "x"))]

