-- -------------------------------------------------------------------
-- Natural Semantics for WHILE 2021.
-- Examen de Lenguajes de Programación. UMA.
-- 1 de febrero de 2021
--
-- Apellidos, Nombre:
-- -------------------------------------------------------------------

module NaturalSemantics where

-- En este fichero solo necesitas completar:
--
--   - 2.a la definición semántica de la sentencia case
--   - 2.a la implementación de la sentencia case
--
-- No modifiques el resto del código. Puedes probar
-- tu implementación con la función run, definida al final.

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

import           While21
import           While21Parser

updateState :: State -> Var -> Z -> State
updateState s y v x = if y == x then v else s x

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

-- representation of the transition relation <S, s> -> s'

nsStm :: Config -> Config

-- x := a

nsStm (Inter (Ass x a) s) = Final (updateState s x (aVal a s))

-- skip

nsStm (Inter Skip s) = Final s

-- s1; s2

nsStm (Inter (Comp ss1 ss2) s) = Final s''
  where
    Final s'  = nsStm (Inter ss1 s)
    Final s'' = nsStm (Inter ss2 s')

-- if b then s1 else s2

nsStm (Inter (If b ss1 ss2) s)
  | bVal b s = Final s'
  where
    Final s' = nsStm (Inter ss1 s)

nsStm (Inter (If b ss1 ss2) s)
  | not(bVal b s) = Final s'
  where
    Final s' = nsStm (Inter ss2 s)

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ARRIBA
----------------------------------------------------------------------

-- case a of

-- |----------------------------------------------------------------------
-- | Exercise 2.a
-- |----------------------------------------------------------------------

-- | Define the Natural Semantics of the case statement.

{-

    Completa la definición semántica de la sentencia case.

  Regla 1: Si encontramos una etiqueta n_ij cuyo valor coincida con el de a, se ejecuta la sentencia S_i correspondiente y se ignora el resto de casos.

                          <S, s> -> s'
  [Case1NStt]  ------------------------------------    if LC = (LL : S LC') and (A[a]s isElem lista) = tt
                     <Case a of LC end, s> -> s'

                       <case a of LC' end, s> -> s'
  [Case1NSff]  -----------------------------------------  if LC = (LL : S LC') and (A[a]s isElem lista) = ff
                       <Case a of LC end, s> -> s'

    Regla 2:  Si ninguna etiqueta n_ij coincide con el valor de a y al final aparece un caso default, se ejecuta la sentencia S_d
    
                          < S, s> -> s_d
  [Case2NS] ---------------------------------------- if LC = default:S
                     < Case a of LC, s > -> s_d

    Regla 3:  Si ninguna etiqueta n_ij coincide con el valor de a y no aparece un caso default, se aborta la ejecución del programa.

  [Case3NS] -------------------------------------- if LC = End
                <Case a of LC end, s> -> error

-}

-- |----------------------------------------------------------------------
-- | Exercise 2.a
-- |----------------------------------------------------------------------

-- | Implement the Natural Semantics of the case statement.

-- Regla 1:
nsStm (Inter (Case aexp (LabelledStm labels stm rest)) s)
  | elem (aVal aexp s) labels = Final s1 -- Si la expresión aritmética coincide con alguna de las etiquetas, se ejecuta la sentencia Si correspondiente
  | otherwise = Final s2 -- Si no coincide, se ignora el resto de casos
     where 
        Final s1 = nsStm (Inter stm s)
        Final s2 = nsStm (Inter (Case aexp rest) s)
-- | evalAexpList aexp labels s = nsStm (Inter stm s) 

-- Regla 2:
nsStm (Inter (Case aexp (Default stm)) s) = Final s1
  where
     Final s1 = nsStm (Inter stm s)

-- Regla 3:
nsStm (Inter (Case aexp EndLabelledStms) s) = error "No se encontró coincidencia y no hay caso default."

-- Función para evaluar si la expresión aritmética coincide con alguna de las etiquetas
-- evalAexpList :: Aexp -> [Integer] -> State -> Bool
-- evalAexpList aexp [] s = False
-- evalAexpList aexp (n:ns) s = (aVal aexp s) == n || evalAexpList aexp ns s

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- | Run the While program stored in filename and show final values of variables.
--   For example: run "TestCase.w"

run :: String -> IO()
run filename =
  do
     (_, vars, stm) <- parser filename
     let Final s = nsStm (Inter stm (const 0))
     print $ showState s vars
     where
       showState s = map (\ v -> v ++ "->" ++ show (s v))
