-- -------------------------------------------------------------------
-- Structural Operational Semantics for WHILE 2021.
-- Examen de Lenguajes de Programación. UMA.
-- 1 de febrero de 2021
--
-- Apellidos, Nombre: Isidro
-- -------------------------------------------------------------------

module StructuralSemantics where

-- En este fichero solo necesitas completar:
--
--   - 2.b la definición semántica de la sentencia case
--   - 2.b la implementación de la sentencia case
--
-- No modifiques el resto del código. Puedes probar
-- tu implementación con la función run, definida al final.

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- import           Data.List.HT  (takeUntil)

import           While21
import           While21Parser

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s
            | Stuck Stm State  -- <S, s>

isFinal :: Config -> Bool
isFinal (Final _) = True
isFinal _         = False

isInter :: Config -> Bool
isInter (Inter _ _) = True
isInter _           = False

isStuck :: Config -> Bool
isStuck (Stuck _ _) = True
isStuck _           = False

update :: State -> Var -> Z -> State
update s y v x = if y == x then v else s x


-- representation of the transition relation <S, s> -> s'

sosStm :: Config -> Config

-- x := a

sosStm (Inter (Ass x a) s) = Final (update s x (aVal a s))

-- skip

sosStm (Inter Skip s) = Final s

-- s1; s2

sosStm (Inter (Comp ss1 ss2) s)
  | isFinal next = Inter ss2 s'
  where
    next = sosStm (Inter ss1 s)
    Final s' = next

sosStm (Inter (Comp ss1 ss2) s)
  | isStuck next = Stuck (Comp stm ss2) s'
  where
    next = sosStm (Inter ss1 s)
    Stuck stm s' = next

sosStm (Inter (Comp ss1 ss2) s)
  | isInter next = Inter (Comp ss1' ss2) s'
  where
    next = sosStm (Inter ss1 s)
    Inter ss1' s' = next

-- if b then s1 else s2

sosStm (Inter (If b ss1 ss2) s)
  | bVal b s = Inter ss1 s

sosStm (Inter (If b ss1 ss2) s)
  | not (bVal b s) = Inter ss2 s

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ARRIBA
----------------------------------------------------------------------

-- case a of

-- |----------------------------------------------------------------------
-- | Exercise 2.b
-- |----------------------------------------------------------------------

-- | Define the Structural Operational Semantics of the case statement.

{-
    Completa la definición semántica de la sentencia case.

    Regla 1: Si encontramos una etiqueta n_ij cuyo valor coincida con el de a, se ejecuta la sentencia S_i correspondiente y se ignora el resto de casos.
                         
             [Case1SOStt]  ---------------------------------    if LC = (LL : S LC') and (A[a]s isElem lista) = tt
                              <case a of LC end, s> => <S, s>

                
             [Case1SOSff]  -------------------------------------------------   if LC = (LL : S LC') and (A[a]s isElem lista) = ff
                             <case a of LC end, s> => <case a of LC' end, s>

    Regla 2:  Si ninguna etiqueta n_ij coincide con el valor de a y al final aparece un caso default, se ejecuta la sentencia S_d
                   
              [Case2SOS] ---------------------------------------- if LC = default:S
                              < case a of LC end, s > => s_d

    Regla 3:  Si ninguna etiqueta n_ij coincide con el valor de a y no aparece un caso default, se aborta la ejecución del programa.

              [Case3SOS] ------------------------------------------- if LC = End
                            < case a of LC end, s > => < abort, s > 

-}

-- |----------------------------------------------------------------------
-- | Exercise 2.b
-- |----------------------------------------------------------------------
-- Abort 
sosStm (Inter Abort s) = Stuck Abort s

-- | Implement in Haskell the Structural Semantics of the case statement.

-- Regla 1: 
sosStm (Inter (Case aexp (LabelledStm labels stm rest)) s)
  | elem (aVal aexp s) labels = Inter stm s
  | otherwise = Inter (Case aexp rest) s -- Si no coincide, se ignora el resto de casos
  -- | evalAexpList aexp labels s = Inter stm s
-- Regla 2: 
sosStm (Inter (Case aexp (Default stm)) s) = Inter stm s

-- Regla 3: 
sosStm (Inter (Case aexp EndLabelledStms) s) = Inter Abort s

-- Función auxiliar para evaluar si la expresión aritmética coincide con alguna de las etiquetas
-- evalAexpList :: Aexp -> [Integer] -> State -> Bool
-- evalAexpList aexp [] s = False
-- evalAexpList aexp (n:ns) s = (aVal aexp s) == n || evalAexpList aexp ns s


----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- | Run the While program stored in filename and show final values of variables

run :: String -> IO()
run filename =
  do
     (_, vars, stm) <- parser filename
     let  dseq = derivSeq stm (const 0)
     putStr $ showDerivSeq vars dseq
     where
      derivSeq st ini = takeUntil (not . isInter) (iterate sosStm (Inter st ini))
      showDerivSeq vars dseq = unlines (map showConfig dseq)
         where
           showConfig (Final s) = "Final state:\n" ++ unlines (showVars s vars)
           showConfig (Stuck stm s) = "Stuck state:\n" ++ show stm ++ "\n" ++ unlines (showVars s vars)
           showConfig (Inter ss s) = show ss ++ "\n" ++ unlines (showVars s vars)
           showVars s vs = map (showVal s) vs
           showVal s x = " s(" ++ x ++ ")= " ++ show (s x)


takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x       = []
  | otherwise = x : takeUntil p xs
