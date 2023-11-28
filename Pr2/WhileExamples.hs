module WhileExamples where

import           Aexp
import           Bexp
import           State
import           While

swap :: Stm
swap = Comp (Ass "z" (V "x"))
      (Comp (Ass "x" (V "y"))
            (Ass "y" (V "z")))

swapInit :: Var -> Z
swapInit "x" = 5
swapInit "y" = 7
swapInit _   = 0

factorial :: Stm
factorial = Comp (Ass "y" (N "1"))
                 (While (Neg (Equ (V "x") (N "1")))
                    (Comp (Ass "y" (Mult (V "y") (V "x")))
                          (Ass "x" (Sub (V "x") (N "1")))))

factorialInit :: State
factorialInit "x" = 3
factorialInit _   = 0
