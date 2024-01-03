{-|

Programming Languages
Fall 2023

Implementation in Haskell of the Proc Language described in
Chapter 2 of Nielson & Nielson, Semantics with Applications

Author: Isidro

-}

module Exercises04 where

import           Aexp
import           NaturalSemantics
import           State
import           While
import           WhileParser

-- |----------------------------------------------------------------------
-- | Exercise 1 - Local Variable Declarations
-- |----------------------------------------------------------------------

-- | Exercise 1.1 - update envV and sto

-- | 'NaturalSemantics.hs' contains definitions for locations, stores,
-- | and variable environments.
-- | Complete the definitions of the functions 'updateV' and 'updateS'
-- | in 'NaturalSemantics.hs'.

-- | Exercise 1.2 - natural semantics for variable declarations

-- | 'NaturalSemantics.hs' defines the algebraic data type 'ConfigD' to
-- | represent configurations of the transition system ->_D for variable
-- | declarations using locations.

-- | Modify 'NaturalSemantics.hs' and complete the definition of
-- | function 'nsDecV' that implements the transition system ->_D.

-- | The code below tests your definitions.

-- | First, we initialize the variable environment and the store:

-- note that global variables are not allowed in WHILE
initEnvV :: EnvVar
initEnvV x = error $ "undefined variable " ++ x

-- note that accessing a non-allocated location yields an error
initStore :: Store
initStore l
  | l == next = 1
  | otherwise = error $ "undefined location " ++ show l

-- | Then, we define some variable declarations:

declarations :: DecVar
declarations =  Dec "x" (N "5")                    -- var x:= 5;
               (Dec "y" (N "2")                    -- var y:= 2;
               (Dec "z" (Mult (V "x") (V "y"))     -- var z:= x * y;
               (Dec "x" (Add (V "x") (N "1"))      -- var x:= 1;
                EndDec)))

-- | and a function 'showDecV' that shows the variables declared in a
-- | 'DecVar'. For each variable 'v' in the list 'vars', it shows
-- | both its location and value:

showDecV :: DecVar -> EnvVar -> Store -> [Var] -> String
showDecV decs env sto vars = foldr (showVar env' sto') [] vars
  where
     FinalD env' sto' = nsDecV (InterD decs env sto)
     showVar env sto x s = "var " ++ x ++ " loc " ++ show (env' x) ++ " val " ++ show (sto' . env' $ x) ++ "\n" ++ s

-- | Finally, we have a simple test for variable declarations:

testVarDec :: IO()
testVarDec = putStr $ showDecV declarations initEnvV initStore ["x", "y", "z"]

-- | and the expected output:

{-

> testVarDec
var x loc 4 val 6  -- note that the first declaration of x is shadowed
var y loc 2 val 2
var z loc 3 val 10

-}

-- |----------------------------------------------------------------------
-- | Exercise 2 - Procedure Declarations
-- |----------------------------------------------------------------------

-- | 'NaturalSemantics.hs' defines an algebraic data type 'EnvProc' to
-- | represent procedure environments.

-- | Exercise 2.1 - update envP
-- |
-- | Complete the definition of function 'updP' in 'NaturalSemantics.hs'.

-- | Exercise 2.2 - look up procedure definitions
-- |
-- | Complete the definition of 'envProc' in 'NaturalSemantics.hs'.
-- | 'envProc envP p' looks up a definition for procedure 'p' in  the
-- | procedure environment 'envP'. If 'p' is defined in 'envP', then
-- | 'envProc' returns a tuple (s, envV', envP') with 's', the body of 'p',
-- | and the snapshots of the variable and procedure environments 'envV''
-- | and 'envP'' associated to 'p'. If 'p' is not defined in 'envP', the
-- | function raises an error "undefined procedure p".

-- | The code below tests your definitions.

-- | First, we initialize the procedure environment:

initEnvP :: EnvProc
initEnvP = EmptyEnvProc

-- | Then, we define some procedure declarations:

procedures :: DecProc
procedures =  Proc "p" Skip      -- proc p is skip;
             (Proc "q" Skip      -- proc q is skip;
             (Proc "r" Skip      -- proc r is skip;
              EndProc))

-- | and the function 'showDecP' that shows the procedures declared in
-- | a 'DecProc'. For each procedure 'p', it shows the other procedures
-- | it knows (i.e. those that can be invoked from 'p').

showDecP :: DecProc -> String
showDecP procs =
   showDecP' $ updP procs undefined initEnvP
   where
      showDecP' EmptyEnvProc = ""
      showDecP' (EnvP p s envV envP envP') = p ++ " knows " ++ knows envP ++ "\n" ++ showDecP' envP'
      knows EmptyEnvProc               = ""
      knows (EnvP p s envV envP envP') = p ++ " " ++ knows envP'
--      showDecP' (EnvP p s envV envP envP') = p ++ "\n" ++ showDecP' envP'

-- | Finally, we have a simple test for procedure declarations:

testProcDec :: IO()
testProcDec = putStr $ showDecP procedures

-- | and the expected output:

{-

> testProcDec
r knows q p
q knows p
p knows

-}

-- |----------------------------------------------------------------------
-- | Exercise 3 - Natural Semantics for While
-- |----------------------------------------------------------------------

-- | Finally, we can implement the natural semantics for WHILE.
-- | 'NaturalSemantics.hs' defines the algebraic data type
-- | 'Config' that represents the configurations of the transition
-- | system.

-- | Complete the definition of the transition relation 'nsStm'.
-- | Note that there are two rules for 'call p', depending on whether
-- | WHILE allows recursive procedures or not. Code just the rule
-- | that supports recursive calls.

-- | The code below tests your definitions.

-- | The function 'showStore' shows the contents of a 'Store' (i.e. a
-- | memory dump). Recall that variable names are missing, but you can
-- | relate memory cells to variables by numbering the variables from 1.

showStore :: Store -> [(Integer, Integer)]
showStore sto = [ (l, v) | l <- [0..sto next - 1], let v = sto l ]

-- | Use the function 'run' below to execute the While programs 'CallTest.w'
-- | and 'RecursiveFactorial.w' in the directory 'Examples' to check your implementation
-- | of the Natural Semantics. For example:
-- |
-- |  > run "Examples/CallTest.w"

-- | Run the While program stored in filename and show the final content of the store
run :: FilePath -> IO()
run filename =
  do
     (program, _, stm) <- parser filename
     let Final store = nsStm emptyEnvV EmptyEnvProc (Inter stm emptyStore)
     putStrLn $ "Program " ++ program ++ " finalized."
     putStr "Memory dump: "
     print $ showStore store
  where
      emptyEnvV x = error $ "undefined variable " ++ x
      emptyStore l
         | l == next = 1
         | otherwise = error $ "undefined location " ++ show l
