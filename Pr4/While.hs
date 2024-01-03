-- While.hs - abstract syntax of WHILE

module While where

import           Aexp
import           Bexp
import           State

-- -------------------------------------------------------------------
-- Abstract syntax of statements
-- -------------------------------------------------------------------

type Pname = String

data DecVar = Dec Var Aexp DecVar
            | EndDec
            deriving Show

data DecProc = Proc Pname Stm DecProc
             | EndProc
             deriving Show

data Stm = Ass Var Aexp
         | Skip
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         | Block DecVar DecProc Stm  -- | todo
         | Call Pname                -- | todo
         deriving Show
