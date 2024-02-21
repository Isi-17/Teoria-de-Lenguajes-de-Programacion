{-|

Programming Languages
Fall 2023

Implementation of the Natural Semantics of the WHILE Language

Author: Isidro Javier Garcia Fernandez

-}

module Exercises02 where

import           NaturalSemantics
import           Test.HUnit       hiding (State)

import           Aexp
import           Bexp
import           State
import           While
import           WhileExamples
import           WhileParser
import           Expressions


-- -- semantic function for natural semantics
-- sNs :: Stm -> State -> State
-- sNs ss s = s'
--   where Final s' = nsStm (Inter ss s)

-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------
-- | The function 'sNs' returns the final state of the execution of a
-- | WHILE statement 'st' from a given initial state 's'. For example:

execFactorial :: State
execFactorial = sNs factorial factorialInit

-- | returns the final state:
-- |
-- |    s x = 1
-- |    s y = 6
-- |    s _ = 0
-- |
-- | Since a state is a function it cannot be printed thus you cannot
-- | add 'deriving Show' to the algebraic data type 'Config'.
-- | The goal of this exercise is to define a function to "show" a state
-- | thus you can inspect the final state yielded by the natural semantics
-- | of WHILE.

-- | Exercise 1.1
-- | Define a function 'showState' that given a state 's' and a list
-- | of variables 'vs' returns a list of strings showing the bindings
-- | of the variables mentioned in 'vs'. For example, for the state
-- | 's' above we get:
-- |
-- |    showState s ["x"] = ["x -> 1"]
-- |    showState s ["y"] = ["y -> 6"]
-- |    showState s ["x", "y"] = ["x -> 1", "y -> 6"]
-- |    showState s ["y", "z", "x"] = ["y -> 6", "z -> 0", "x -> 1"]

showState :: State -> [Var] -> [String]
showState s [] = []
showState s (x:xs) = (x ++ "->" ++ show (s x)) : showState s xs 

-- | Test your function with HUnit.

testshowState :: Test
testshowState = TestList [ "showState s [\"x\"]" ~: ["x->1"] ~=? showState execFactorial ["x"]
                         , "showState s [\"y\"]" ~: ["y->6"] ~=? showState execFactorial ["y"]
                         , "showState s [\"x\", \"y\"]" ~: ["x->1", "y->6"] ~=? showState execFactorial ["x", "y"]
                         , "showState s [\"y\", \"z\", \"x\"]" ~: ["y->6", "z->0", "x->1"] ~=? showState execFactorial ["y", "z", "x"]
                         ]

-- | Using the function 'sNs' to execute a WHILE program is handy a bit awkward.
-- | The WHILE statement must be provided in abstract syntax and the initial
-- | state must be explicitly given and inspected.
-- |
-- | The 'run' function allows to execute a WHILE program stored in a file
-- | in concrete syntax and reports the final value of the variables mentioned
-- | in the program header. For example:
-- |
-- |    > run "Examples/Factorial.w"
-- |    Program Factorial finalized.
-- |    Final State: ["x->0","y->120"]

-- | Run the WHILE program stored in filename and show final values of variables
run :: FilePath -> IO()
run filename =
  do
     (programName, vars, stm) <- parser filename
     let Final s = nsStm (Inter stm (const 0))
     putStrLn $ "Program " ++ programName ++ " finalized."
     putStr "Final State: "
     print $ showState s vars

-- | Exercise 1.2
-- | Use the function 'run' to execute the WHILE programs 'Factorial.w' and 'Divide.w'
-- | in the directory 'Examples' to check your implementation of the Natural Semantics.
-- | Write a few more WHILE programs. For example, write a WHILE program
-- | "Power.w" to compute x^y.

-- > run "Examples/Divide.w"
-- Program Divide finalized.
-- Final State: ["q->3","r->2","error->0"]

-- pow(x, y) = x^y (x = 3, y = 4)

-- > run "Examples/Power.w"
-- Program Power finalized.
-- Final State: ["x->3","y->0","z->81"]

-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'repeat S until b' statement.
-- | The file Examples/FactorialRepeat.w contains a simple program to
-- | compute the factorial with a 'repeat until' loop.

-- > run "Examples/FactorialRepeat.w"
-- Program FactorialRepeat finalized.
-- Final State: ["x->-1","fac->0"]

{- Formal definition of 'repeat S until b'

-- | Exercise 2.1
-- | Define the natural semantics of this new statement. You are not allowed
-- | to rely on the 'while b do S' statement.
-- The answer is in the file While.hs
    Repeat Stm Bexp
-}


-- | Exercise 2.2
-- | Extend the definition of 'nsStm' in module NaturalSemantics.hs
-- | to include the 'repeat S until b' statement.
-- The answer is in the file NaturalSemantics.hs

-- | Exercise 2.3
-- | Write a couple of WHILE programs that use the 'repeat' statement and
-- | test your functions with HUnit and 'run'.

-- > run "Examples/PowerRepeat.w"
-- Program Power finalized.
-- Final State: ["x->3","y->0","z->81"]

-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'for x:= a1 to a2 do S'
-- | statement.
-- | The file Examples/FactorialFor.w contains a simple program to compute
-- | the factorial with a 'for' loop.
-- | The file Examples/TestsFor.w contains a more contrived example illustrating
-- | some subtle points of the semantics of the for loop.

-- | Exercise 3.1
-- | Define the natural semantics of this new statement. You are not allowed
-- | to rely on the 'while b do S' or the 'repeat S until b' statements.

{- Formal definition of 'for x:= a1 to a2 do S'
-- The answer is in the file While.hs
  For Var Aexp Aexp Stm     
-}

-- | Exercise 3.2
-- | Extend  the definition 'nsStm' in module NaturalSemantics.hs
-- | to include the 'for x:= a1 to a2 do S' statement.

  -- > run "Examples/TestFor.w"
  -- Program TestFor finalized.
  -- Final State: ["x->6","ac->720","i->6","start->13","stop->-6"]

  -- > run "Examples/FactorialFor.w"    
  -- Program FactorialFor finalized.
  -- Final State: ["x->5","fac->120"]

-- | Exercise 3.3
-- | Write a couple of WHILE programs that use the 'for' statement
-- | and test your functions with HUnit and 'run'.

  -- > run "Examples/PowerFor.w"    
  -- Program Power finalized.
  -- Final State: ["x->3","y->4","z->81"]

-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------

-- | Define the semantics of arithmetic expressions (Aexp) by means of
-- | natural semantics. To that end, define an algebraic datatype 'ConfigAexp'
-- | to represent the configurations, and a function 'nsAexp' to represent
-- | the evaluation judgement.

-- representation of configurations for Aexp

data ConfigAExp = InterAExp Aexp State  -- <a, s>
                | FinalAExp Z           -- z
                -- deriving(Eq, Show)

-- representation of the evaluation judgement <a, s> -> z

nsAexp :: ConfigAExp -> ConfigAExp
nsAexp (FinalAExp x) = FinalAExp x
nsAexp (InterAExp (N n) s) = FinalAExp (read n)
nsAexp (InterAExp (V x) s) = FinalAExp (s x)
nsAexp (InterAExp (Add a1 a2) s) = FinalAExp (z1 + z2)
  where
    FinalAExp z1 = nsAexp (InterAExp a1 s)
    FinalAExp z2 = nsAexp (InterAExp a2 s)
nsAexp (InterAExp (Sub a1 a2) s) = FinalAExp (z1 - z2)
  where
    FinalAExp z1 = nsAexp (InterAExp a1 s)
    FinalAExp z2 = nsAexp (InterAExp a2 s)
nsAexp (InterAExp (Mult a1 a2) s) = FinalAExp (z1 * z2)
  where
    FinalAExp z1 = nsAexp (InterAExp a1 s)
    FinalAExp z2 = nsAexp (InterAExp a2 s)



-- | Define the semantics of arithmetic expressions (Bexp) by means of
-- | natural semantics. To that end, define an algebraic datatype 'ConfigBexp'
-- | to represent the configurations, and a function 'nsBexp' to represent
-- | the evaluation judgement.

-- representation of configurations for Bexp

data ConfigBExp = InterBExp Bexp State  -- <b, s>
                | FinalBExp Bool        -- b
                -- deriving(Eq, Show)

-- representation of the evaluation judgement <b, s> -> z

nsBexp :: ConfigBExp -> ConfigBExp
nsBexp (FinalBExp b) = FinalBExp b
nsBexp (InterBExp TRUE _) = FinalBExp True
nsBexp (InterBExp FALSE _) = FinalBExp False
nsBexp (InterBExp (Equ a1 a2) s) = FinalBExp (z1 == z2)
  where
    FinalAExp z1 = nsAexp (InterAExp a1 s)
    FinalAExp z2 = nsAexp (InterAExp a2 s)
nsBexp (InterBExp (Leq a1 a2) s) = FinalBExp (z1 <= z2)
  where
    FinalAExp z1 = nsAexp (InterAExp a1 s)
    FinalAExp z2 = nsAexp (InterAExp a2 s)
nsBexp (InterBExp (Neg b) s) = FinalBExp (not b')
  where
    FinalBExp b' = nsBexp (InterBExp b s)
nsBexp (InterBExp (And b1 b2) s) = FinalBExp (b1' && b2')
  where
    FinalBExp b1' = nsBexp (InterBExp b1 s)
    FinalBExp b2' = nsBexp (InterBExp b2 s)



-- | Test your function with HUnit. Inspect the final values of at least
-- | four different evaluations.
-- testnsAexp :: Test
-- testnsAexp = TestList [ "Test1" ~: 1 ~=? val where FinalAExp val = nsAexp (InterAExp (N "1") sInit)
--                       , "Test2" ~: 3 ~=? val where FinalAExp val = nsAexp (InterAExp (V "x") sInit)
--                       , "Test3" ~: 3 ~=? val where FinalAExp val = nsAexp (InterAExp (Add (N "1") (N "2")) sInit)
--                       , "Test4" ~: 3 ~=? val where FinalAExp val = nsAexp (InterAExp (Sub (V "x") (V "y")) sInit) -- y = 0 en sInit
--                       , "Test5" ~: 9 ~=? val where FinalAExp val = nsAexp (InterAExp (Mult (V "x") (V "x")) sInit)
--                       ]

-- |----------------------------------------------------------------------
-- | Exercise 5
-- |----------------------------------------------------------------------

-- | In the statement 'for x:= a1 to a2 S' the variable 'x' is the control
-- | variable. Some programming languages protect this variable in that
-- | it cannot be assigned to in the body of the loop, S.
-- |
-- | For example, the program below:
-- |
-- |    y := 1;
-- |    for x:= 1 to 10 do begin
-- |       y := y * x;
-- |       x := x + 1    // assignment to control variable
-- |    end
-- |
-- | would be rejected by languages enforcing such a restriction.
-- | Note that this check is performed before the program is executed,
-- | and therefore is a static semantics check.

-- | Exercise 5.1
-- | Define the static semantics by means of a judgement that is valid
-- | if and only if the program does not overwrite its control variables.
-- | Use axioms and inference rules to validate your judgements.

{-
-}

-- | Exercise 5.2
-- | Define a function 'forLoopVariableCheck' that implements the static
-- | semantics check above described. Use the function 'analyze'
-- | to check your implementation.

-- | Exercise 1.2
-- | Define a function 'fvStm' that returns the free variables of a WHILE
-- | statement. For example:
-- |
-- | fvStm factorial = ["y","x"]
-- |
-- | Note: the order of appearance is not relevant, but there should not be
-- | duplicates.
purgar :: [Var] -> [Var]
purgar [] = []
purgar (x:xs)
  | elem x xs = purgar xs
  | otherwise = x : purgar xs


fvStm :: Stm -> [Var]
fvStm (Ass x a) = purgar (x : fvAexp a)
fvStm (Skip) = []
fvStm (Comp ss1 ss2) = purgar ((fvStm ss1) ++ (fvStm ss2))
fvStm (If b ss1 ss2) = purgar((fvBexp b) ++ (fvStm ss1) ++ (fvStm ss2))
fvStm (While b ss) = purgar((fvBexp b) ++ (fvStm ss))
fvStm (Repeat ss b) = purgar((fvStm ss) ++ (fvBexp b))
fvStm (For x a1 a2 ss) = purgar((fvAexp a1) ++ (fvAexp a2) ++ (fvStm ss))


testfvStm :: Test
testfvStm = TestList [ "Test1" ~: [] ~=? fvStm (Skip),
                        "Test2" ~: ["x"] ~=? fvStm (Ass "x" (N "1")),
                        "TestPower" ~: ["z", "x", "y"] ~=? fvStm(Comp (Ass "z" (N "1"))
                                                                  (While (Neg (Equ (V "y") (N "0")))
                                                                      (Comp (Ass "z" (Mult (V "z") (V "x")))
                                                                            (Ass "y" (Sub (V "y") (N "1"))))))
                      ]

forLoopVariableCheck :: Stm -> Bool
forLoppVariableCheck (For x a1 a2 ss) = not (elem x (fvStm ss)) && forLoopVariableCheck ss -- Si x no es una variable del cuerpo del bucle
forLoopVariableCheck (Repeat ss b) = forLoopVariableCheck ss
forLoopVariableCheck (While b ss) = forLoopVariableCheck ss
forLoopVariableCheck (If b ss1 ss2) = (forLoopVariableCheck ss1) && (forLoopVariableCheck ss2)
forLoopVariableCheck (Comp ss1 ss2) = (forLoopVariableCheck ss1) && (forLoopVariableCheck ss2)
forLoopVariableCheck _ = True -- Ass, Skip

testforLoopVariableCheck :: Test
testforLoopVariableCheck = TestList ["Test1" ~: True ~=? forLoopVariableCheck (Skip),
                                      "Test2" ~: True  ~=? forLoopVariableCheck (Ass "x" (N "1")),
                                      "TestPower" ~: True ~=? forLoopVariableCheck (Comp (Ass "z" (N "1"))
                                                                                    (While (Neg (Equ (V "y") (N "0")))
                                                                                        (Comp (Ass "z" (Mult (V "z") (V "x")))
                                                                                              (Ass "y" (Sub (V "y") (N "1")))))),
                                      "TestNestedFor" ~: True ~=? forLoopVariableCheck (For "x" (N "1") (N "3") 
                                                                                          (For "y" (N "1") (N "3") 
                                                                                              (Ass "z" (N "1")))),
                                      "TestNestedFor2" ~: False ~=? forLoopVariableCheck (For "x" (N "1") (N "3") 
                                                                                            (For "x" (N "1") (N "3")
                                                                                              (Ass "x" (N "1"))))
                                    ]

-- | Analyze the WHILE program stored in filename and show results
analyze :: FilePath -> IO()
analyze filename =
  do
     (program, _, stm) <- parser filename
     putStrLn $ "Analyzing program " ++ program
     let ok = forLoopVariableCheck stm
     if ok then putStrLn "Program accepted"
     else putStrLn "Program rejected"


-- | Convert concrete syntax to abstract syntax
concreteToAbstract :: FilePath -> FilePath -> IO()
concreteToAbstract inputFile outputFile =
  do
    (_, _, stm) <- parser inputFile
    let s = show stm              -- | have 'show' replaced by a pretty printer
    if null outputFile
      then putStrLn s
      else writeFile outputFile s


power :: Stm
power = Comp (Ass "z" (N "1"))
                 (While (Neg (Equ (V "y") (N "0")))
                    (Comp (Ass "z" (Mult (V "z") (V "x")))
                          (Ass "y" (Sub (V "y") (N "1")))))


-- |----------------------------------------------------------------------
-- | Exercise 6
-- |----------------------------------------------------------------------

-- | Given the algebraic data type 'DerivTree' to represent derivation trees
-- | of the natural semantics:
-- data Config = Inter Stm State  -- <S, s>
--             | Final State      -- s

data Transition = Config :-->: State

data DerivTree = AssNS     Transition
               | SkipNS    Transition
               | CompNS    Transition DerivTree DerivTree
               | IfTTNS    Transition DerivTree
               | IfFFNS    Transition DerivTree
               | WhileTTNS Transition DerivTree DerivTree
               | WhileFFNS Transition
               | RepeatTTNS Transition
               | RepeatFFNS Transition DerivTree DerivTree
               | ForTTNS    Transition DerivTree DerivTree DerivTree
               | ForFFNS    Transition
               

-- | and the function 'getFinalState' to access the final state of the root
-- | of a derivation tree:

getFinalState :: DerivTree -> State
getFinalState (AssNS  (_ :-->: s))         = s
getFinalState (SkipNS (_ :-->: s))         = s
getFinalState (CompNS (_ :-->: s) _ _ )    = s
getFinalState (IfTTNS (_ :-->: s) _ )      = s
getFinalState (IfFFNS (_ :-->: s) _ )      = s
getFinalState (WhileTTNS (_ :-->: s) _ _ ) = s
getFinalState (WhileFFNS (_ :-->: s))      = s
getFinalState (RepeatTTNS (_ :-->: s))     = s
getFinalState (RepeatFFNS (_ :-->: s) _ _ ) = s
getFinalState (ForTTNS (_ :-->: s) _ _ _)  = s
getFinalState (ForFFNS (_ :-->: s))        = s


-- | Define a function 'nsDeriv' that given a WHILE statement 'st' and an
-- | initial state 's' returns corresponding derivation tree.

nsDeriv :: Stm -> State -> DerivTree
nsDeriv (Ass x a) s = AssNS ((Inter (Ass x a) s) :-->: s')
  where 
    Final s' = nsStm (Inter (Ass x a) s)
nsDeriv (Skip) s = SkipNS ((Inter Skip s) :-->: s)
nsDeriv (Comp ss1 ss2) s = CompNS ((Inter (Comp ss1 ss2) s) :-->: s'') (nsDeriv ss1 s) (nsDeriv ss2 s')
  where 
    Final s' = nsStm (Inter ss1 s)
    Final s'' = nsStm (Inter ss2 s') 
nsDeriv (If b ss1 ss2) s
  | bVal b s == True = IfTTNS ((Inter (If b ss1 ss2) s) :-->: s1) (nsDeriv ss1 s) -- true, <S1, s> -> s1
  | otherwise = IfFFNS ((Inter (If b ss1 ss2) s) :-->: s2) (nsDeriv ss2 s) -- false, <S2, s> -> s2
  where
    Final s1 = nsStm (Inter ss1 s)
    Final s2 = nsStm (Inter ss2 s)
nsDeriv (While b ss) s
  | bVal b s == True = WhileTTNS ((Inter (While b ss) s) :-->: s') (nsDeriv ss s) (nsDeriv (While b ss) s')
  | otherwise = WhileFFNS ((Inter (While b ss) s) :-->: s)
  where 
    Final s' = nsStm (Inter ss s) 
nsDeriv (Repeat ss b) s
  | bVal b s == False = RepeatFFNS ((Inter (Repeat ss b) s) :-->: s') (nsDeriv ss s) (nsDeriv (Repeat ss b) s')
  | otherwise = RepeatTTNS ((Inter (Repeat ss b) s) :-->: s)
  where 
    Final s' = nsStm (Inter ss s)
nsDeriv (For x a1 a2 ss) s
  | bVal (Leq a1 a2) s == True = ForTTNS ((Inter (For x a1 a2 ss) s) :-->: s''') (nsDeriv (Ass x a1) s) (nsDeriv ss s') (nsDeriv (For x (Add v1 (N "1")) v2 ss) s'')
  | otherwise = ForFFNS ((Inter (For x a1 a2 ss) s) :-->: s)
  where 
    Final s' = nsStm (Inter (Ass x a1) s)
    Final s'' = nsStm (Inter ss s')
    Final s''' = nsStm (Inter (For x (Add v1 (N "1")) v2 ss) s'')
    v1 = N (show (aVal a1 s))
    v2 = N (show (aVal a2 s))

-- | Define random in NaturalSemantics
--  Idea:                                  
--             < S1, x> -> 0                                < S2, x> -> x+1
--        ------------------------------              ------------------------------
--          < random(x), s> ->  0                       < random(x), s> ->  <random(x+1), s>

