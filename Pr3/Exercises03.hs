{-|

Programming Languages
Fall 2023

Implementation of the Structural Operational Semantics of the WHILE Language

Author: Isidro

-}


module Exercises03 where

import           Test.HUnit          hiding (State)

import           Aexp
import           Bexp
import           State
import           StructuralSemantics
import           While
import           WhileExamples
import           WhileParser

-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------

-- | Given the type synonym 'DerivSeq' to represent derivation sequences
-- | of the structural operational semantics:

type DerivSeq = [Config]

-- | Define a function 'derivSeq' that given a WHILE statement 'st' and an
-- | initial state 's' returns the corresponding derivation sequence:

derivSeq :: Stm -> State -> DerivSeq
derivSeq ss s
  | isFinal next = [Inter ss s] ++ [Final s']
  where 
    next = sosStm (Inter ss s)
    Final s' = next

derivSeq ss s
  | isInter next = [Inter ss s] ++ derivSeq ss' s'
  where 
    next = sosStm (Inter ss s)
    Inter ss' s' = next

-- | The function 'showDerivSeq' returns a String representation  of
-- | a derivation sequence 'dseq'. The 'vars' argument is a list of variables
-- | that holds the variables to be shown in the state:

showDerivSeq :: [Var] -> DerivSeq -> String
showDerivSeq vars dseq = unlines (map showConfig dseq)
  where
    showConfig (Final s) = "Final state:\n" ++ unlines (showVars s vars)
    showConfig (Stuck stm s) = "Stuck state:\n" ++ show stm ++ "\n" ++ unlines (showVars s vars)
    showConfig (Inter ss s) = show ss ++ "\n" ++ unlines (showVars s vars)
    showVars s vs = map (showVal s) vs
    showVal s x = " s(" ++ x ++ ")= " ++ show (s x)

-- | Use the function 'run' below to execute the WHILE programs 'Divide.w'
-- | and 'Factorial.w' in the directory 'Examples' to check your implementation
-- | of the Structural Semantics. For example:
-- |
-- |  > run "Examples/Factorial.w"
-- |
-- | Write a few more WHILE programs. For example, write a WHILE program to
-- | compute x^y.

-- | Run the WHILE program stored in filename and show final values of variables

run :: FilePath -> IO()
run filename =
  do
     (_, vars, stm) <- parser filename
     let  dseq = derivSeq stm (const 0)
     putStr $ showDerivSeq vars dseq

-- | The function 'sSoS' below is the semantic function of the
-- | structural operational semantics of WHILE. Given a WHILE statement 'st'
-- | and an initial state 's' returns the final configuration of the
-- | corresponding derivation sequence:

sSos :: Stm -> State -> State
sSos ss s = s'
  where Final s' = last (derivSeq ss s)

-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'repeat S until b' statement.

-- | Exercise 2.1
-- | Define the structural operational semantics of this new statement. You
-- | are not allowed to rely on the 'while b do S' statement.

{- Formal definition of 'repeat S until b'

    [repeat]   --------------------------------------------------------------------------
                <repeat S until b, s> => <S; if b then skip else (repeat S until b), s>

-}

-- | Exercise 2.2
-- | Modify the definition of 'sosStm' in 'StructuralSemantics.hs' to deal
-- | with the 'repeat until' statement.


-- | Exercise 2.3
-- | Write a WHILE program to test your definition of the repeat statement.


-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------

-- | The WHILE language can be extended with a 'for x:= a1 to a2' statement.

-- | Exercise 3.1
-- | Define the structural operational semantics of this new statement. You
-- | are not allowed to rely on the 'while b do s' statement.

{- Formal definition of 'for x:= a1 to a2'

  [for]  -------------------------------------------------------------------------------------------------
          <for x:=a1 to a2 do S, s> => <x:=a1; if (a1<=a2) then (S; for x:=v1+1 to v2 do S) else skip, s>

     v1 = N-1(A[a1]s)
     v2 = N-1(A[a2]s)

-}

-- | Exercise 3.2
-- | Modify the definition of 'sosStm' in 'StructuralSemantics.hs' to deal
-- | with the 'for' statement.


-- | Exercise 3.3
-- | Write a WHILE program to test your definition of the for statement.


-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------

-- | Extend WHILE with the 'Abort' statement. The informal semantics of
-- | 'Abort' is to abruptly stop the execution of the program, similar to
-- | a call to 'exit(0)' in other mainstream languages.

-- | Exercise 4.1
-- | Modify the definition of 'sosStm' in 'StructuralSemantics.hs' to deal
-- | with the 'abort' statement


-- | Exercise 4.2
-- | Define a function 'derivSeqAbort' similar to 'derivSeq' except
-- | that it deals with stuck configurations.

derivSeqAbort :: Stm -> State -> DerivSeq
derivSeqAbort ss s
  | isFinal next = [Inter ss s] ++ [Final s']
    where
      next = sosStm (Inter ss s)
      Final s' = next

derivSeqAbort ss s
  | isInter next = [Inter ss s] ++ (derivSeqAbort ss' s')
    where
      next = sosStm (Inter ss s)
      Inter ss' s' = next

derivSeqAbort ss s
  | isStuck next = [Inter ss s] ++ [Stuck ss' s']
    where
      next = sosStm (Inter ss s)
      Stuck ss' s' = next

-- | Use the function 'runAbort' below to execute the WHILE programs 'Aborti.w'
-- | in the directory 'Examples' to check your implementation of the Structural
-- | Semantics. For example:
-- |
-- |  > run "Examples/Abort0.w"
-- |

-- | Run the WHILE program stored in filename and show final values of variables

runAbort :: FilePath -> IO()
runAbort filename =
  do
     (_, vars, stm) <- parser filename
     let  dseq = derivSeqAbort stm (const 0)
     putStr $ showDerivSeq vars dseq

-- |----------------------------------------------------------------------
-- | Exercise 5
-- |----------------------------------------------------------------------

-- | Implement in Haskell the Structural Operational Semantics for the
-- | evaluation of arithmetic expressions Aexp.

{-
   Structural Operational Semantics for the left-to-right evaluation of
   arithmetic expressions:

   A configuration is either intermediate <Aexp, State> or final Z.

   Note we are abusing notation and write 'n' for both a literal (syntax)
   and an integer (semantics). The same goes for arithmetic operators (+,-,*).

   [N]  ----------------
         < n, s > => n

   [V] ------------------------
        < x, s > => < s x, s >

   [+] -----------------------------  where n3 = n1 + n2
        < n1 + n2, s > => < n3, s >

            < a2, s > => < a2', s >
   [+] ----------------------------------
        < n1 + a2, s > => < n1 + a2', s >

            < a1, s > => < a1', s >
   [+] -----------------------------------
        < a1 + a2, s > => < a1' + a2, s >

   The rules for * and - are similar.

-}

-- | We use the algebraic data type 'AexpConfig' to represent the
-- | configuration of the transition system

data AexpConfig = Redex Aexp State  -- a redex is a reducible expression
                | Value Z           -- a value is not reducible; it is in normal form


-- |----------------------------------------------------------------------
-- | Exercise 5.1
-- |----------------------------------------------------------------------

-- | Define a function 'sosAexp' that given a configuration 'AexpConfig'
-- | evaluates the expression in 'AexpConfig' one step and returns the
-- | next configuration.

sosAexp :: AexpConfig -> AexpConfig

sosAexp (Redex (N n) s) = Value (read n)

sosAexp (Redex (V x) s) = Redex (N (show (s x))) s

-- add
sosAexp (Redex (Add (N n1) (N n2)) s) = Redex (N n3) s
  where n3 = show (read n1 + read n2)

sosAexp (Redex (Add (N n1) a2) s) = Redex (Add (N n1) a2') s
  where Redex a2' s = sosAexp (Redex a2 s)

sosAexp (Redex (Add a1 a2) s) = Redex (Add a1' a2) s
  where Redex a1' s = sosAexp (Redex a1 s)

-- sub
sosAexp (Redex (Sub (N n1) (N n2)) s) = Redex (N n3) s
  where n3 = show (read n1 - read n2)

sosAexp (Redex (Sub (N n1) a2) s) = Redex (Sub (N n1) a2') s
  where Redex a2' s = sosAexp (Redex a2 s)

sosAexp (Redex (Sub a1 a2) s) = Redex (Sub a1' a2) s
  where Redex a1' s = sosAexp (Redex a1 s)

-- mult
sosAexp (Redex (Mult (N n1) (N n2)) s) = Redex (N n3) s
  where n3 = show (read n1 * read n2)

sosAexp (Redex (Mult (N n1) a2) s) = Redex (Mult (N n1) a2') s
  where Redex a2' s = sosAexp (Redex a2 s)

sosAexp (Redex (Mult a1 a2) s) = Redex (Mult a1' a2) s
  where Redex a1' s = sosAexp (Redex a1 s)

-- |----------------------------------------------------------------------
-- | Exercise 5.2
-- |----------------------------------------------------------------------

-- | Given the type synonym 'AexpDerivSeq' to represent derivation sequences
-- | of the structural operational semantics for arithmetic expressions 'Aexp':

isRedex :: AexpConfig -> Bool
isRedex (Redex _ _) = True
isRedex _ = False

isValue :: AexpConfig -> Bool
isValue (Value _) = True
isValue _ = False

type AexpDerivSeq = [AexpConfig]

-- | Define a function 'aExpDerivSeq' that given a 'Aexp' expression 'a' and an
-- | initial state 's' returns the corresponding derivation sequence:

aExpDerivSeq :: Aexp -> State -> AexpDerivSeq
aExpDerivSeq a s
  | isValue next = [Redex a s] ++ [Value n]
    where
      next = sosAexp (Redex a s)
      Value n = next

aExpDerivSeq exp s
  | isRedex next = [Redex exp s] ++ (aExpDerivSeq exp' s')
    where
      next = sosAexp (Redex exp s)
      Redex exp' s' = next

-- | To test your code, you can use the function 'showAexpDerivSeq' that
-- | returns a String representation  of a derivation sequence 'dseq':

showAexpDerivSeq :: [Var] -> AexpDerivSeq -> String
showAexpDerivSeq vars dseq = unlines (map showConfig dseq)
  where
    showConfig (Value n)    = "Final value:\n" ++ show n
    showConfig (Redex ss s) = show ss ++ "\n" ++ unlines (map (showVal s) vars)
    showVal s x = " s(" ++ x ++ ")= " ++ show (s x)

-- | Therefore, you can print the derivation sequence of an 'Aexp' with:

exp1 :: Aexp
exp1 = ( (V "x") `Add` (V "y") ) `Add` (V "z")

exp2 :: Aexp
exp2 =  (V "x") `Add` ( (V "y") `Add` (V "z") )

exp3 :: Aexp
exp3 = Mult (V "x") (Add (V "y") (Sub (V "z") (N "1")))

sExp :: State
sExp "x" = 1
sExp "y" = 2
sExp "z" = 3
sExp  _  = 0

showAexpSeq :: Aexp -> State -> IO()
showAexpSeq a s = putStrLn $ showAexpDerivSeq ["x", "y", "z"] (aExpDerivSeq a s)

-- | Test you code printing derivation sequences for the expressions above as follows:

showExp1 :: IO ()
showExp1 = showAexpSeq exp1 sExp

-- | Convert concrete syntax to abstract syntax

concreteToAbstract :: FilePath -> FilePath -> IO()
concreteToAbstract inputFile outputFile =
  do
    (_, _, stm) <- parser inputFile
    let s = show stm              -- | have 'show' replaced by a pretty printer
    if null outputFile
      then putStrLn s
      else writeFile outputFile s
