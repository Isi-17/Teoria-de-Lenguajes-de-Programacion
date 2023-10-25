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

binVal :: Bin -> Z  -- N : Num -> Z
binVal (MSB bit) 
    | bit == O = 0 
    | bit == I = 1  
binVal (B bin bit) 
  | bit == O = 2*(binVal bin)       --   2 * N[n]
  | otherwise = 2*(binVal bin) + 1  --   2 * N[n] + 1
 

-- | Test your function with HUnit.

testBinVal :: Test
testBinVal = test ["value of zero"  ~: 0 ~=? binVal zero,
                   "value of one"   ~: 1 ~=? binVal one,
                   "value of three" ~: 3 ~=? binVal three,
                   "value of six"   ~: 6 ~=? binVal six]

-- | Define a function 'foldBin' to fold a value of type 'Bin'

foldBin :: (b -> Bit -> b) -> (Bit -> b) -> Bin -> b
foldBin f solBase (MSB bit) = solBase bit
foldBin f solBase (B bin bit) = recfoldBin bin
    where 
      recfoldBin (MSB bit) = solBase bit
      recfoldBin (B bin bit) = f (recfoldBin bin) bit

-- | and use 'foldBin' to define a function 'binVal''  equivalent to 'binVal'.

binVal' :: Bin -> Integer
binVal' = foldBin (\ bin bit -> if bit == O then 2*bin  else 2*bin + 1) (\ bit -> if bit == O then 0 else 1) 


-- | Test your function with HUnit.

-- todo

-- | Define a function 'normalize' that given a binary numeral trims leading zeroes.
-- | For example, normalize (B (B (MSB O) I) I) should return (B (MSB I) I).
-- Arreglar. No funciona.
normalize :: Bin -> Bin
normalize (MSB bit) = MSB bit
normalize (B bin bit) = recnormalize bin
    where 
      recnormalize (MSB bit) = MSB bit
      recnormalize (B bin bit) = if bit == O then recnormalize bin else B bin bit

-- | and use 'foldBin' to define a function 'normalize''  equivalent to 'normalize'.

normalize' :: Bin -> Bin
normalize' = undefined

-- | Test your functions with HUnit.

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 2 - Free variables of expressions
-- |----------------------------------------------------------------------
-- | Define the function 'fvAexp' that computes the set of free variables
-- | occurring in an arithmetic expression. Ensure that each free variable
-- | occurs once in the resulting list.

fvAexp :: Aexp -> [Var]
fvAexp = undefined

-- | Test your function with HUnit.

-- todo

-- | Define the function 'fvBexp' that computes the set of free variables
-- | occurring in a Boolean expression.

fvBexp :: Bexp -> [Var]
fvBexp = undefined

-- | Test your function with HUnit.

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 3 - Substitution of variables in expressions
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Subst' for representing substitutions:

data Subst = Var :->: Aexp

-- | define a function 'substAexp' that takes an arithmetic expression
-- | 'a' and a substitution 'y:->:a0' and returns the substitution a [y:->:a0];
-- | i.e., replaces every occurrence of 'y' in 'a' by 'a0'.

substAexp :: Aexp -> Subst -> Aexp
substAexp = undefined

-- | Test your function with HUnit.

-- todo

-- | Define a function 'substBexp' that implements substitution for
-- | Boolean expressions.

substBexp :: Bexp -> Subst -> Bexp
substBexp = undefined

-- | Test your function with HUnit.

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 4 - Update of state
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Update' for state updates:

data Update = Var :=>: Z

-- | define a function 'update' that takes a state 's' and an update 'x :=> v'
-- | and returns the updated state 's [x :=> v]'

update :: State -> Update -> State
update = undefined

-- | Test your function with HUnit.

-- todo

-- | Define a function 'updates' that takes a state 's' and a list of updates
-- | 'us' and returns the updated states resulting from applying the updates
-- | in 'us' from head to tail. For example:
-- |
-- |    updates s ["x" :=>: 1, "y" :=>: 2, "x" :=>: 3]
-- |
-- | returns a state that binds "x" to 3 (the most recent update for "x").

updates :: State ->  [Update] -> State
updates = undefined

-- | Test your function with HUnit.

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 5 - Folding expressions
-- |----------------------------------------------------------------------
-- | Define a function 'foldAexp' to fold an arithmetic expression

foldAexp :: untyped
foldAexp = undefined

-- | Use 'foldAexp' to define the functions 'aVal'', 'fvAexp'', and 'substAexp''.

aVal' :: Aexp -> State -> Z
aVal' = undefined

fvAexp' :: Aexp -> [Var]
fvAexp' = undefined

substAexp' :: Aexp -> Subst -> Aexp
substAexp' = undefined

-- | Test your functions with HUnit.

-- todo

-- | Define a function 'foldBexp' to fold a Boolean expression and use it
-- | to define the functions 'bVal'', 'fvBexp'', and 'substAexp''.

foldBexp :: untyped
foldBexp = undefined

bVal' :: Bexp -> State -> Bool
bVal' = undefined

fvBexp' :: Bexp -> [Var]
fvBexp' = undefined

substBexp' :: Bexp -> Subst -> Bexp
substBexp' = undefined

-- | Test your functions with HUnit.

-- todo
