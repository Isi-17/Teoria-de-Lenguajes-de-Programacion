----------------------------------------------------------------
-- Lenguajes de Programación
-- 4 del Grado en Ingeniería Informática, mención en Computación
-- Pablo López
----------------------------------------------------------------
-- PARA COMPILAR: C:\Users\isidr\Desktop\TLP\NotasdeClase> .\Repaso.hs
-- Se abre la terminal y podemos trabajar.
module Repaso where

import           Data.Char
import           Data.List
import           Data.Maybe

-- Haskell = Puro + Tipado estático fuerte + Perezoso

-- | 1. Tipos básicos:
----------------------------------------------------------------

-- Los nombres de los tipos empiezan por mayúsculas:
-- Int, Integer, Float, Double, Char, Bool

-- | 2. Definición de funciones: currificación
----------------------------------------------------------------

-- square

{-

   int square(int x) {
      return x * x;
   }

-}

square :: Integer -> Integer -- declaración
square x = x * x             -- definición

-- pythagoras

{-

   int pythagoras(int x, int y) {
      return square(x) + square(y);
   }

-}
-- No se usa los parentesis: square(x)
-- Int es entero y square es para Integer, luego tengo que usar Integer para definir la funcion
-- En consola si pongo: square 5+2 La salida sera: 27 (5*5 + 2)
-- En consola si pongo: square (5+2) La salida sera: 49 (7*7)

pythagoras :: Integer -> Integer -> Integer
pythagoras x y = square x + square y

-- | 3. Condicionales en Haskell
----------------------------------------------------------------

-- El condicional if then else en Haskell:
--
--     1) es una expresión, no una sentencia;
--     2) el else es obligatorio
--     3) los tipos de then y else debe coincidir

maxOf :: Integer -> Integer -> Integer -- predefinida como max
maxOf x y = if x >= y then x else y
-- signo (predefinida como signum)

{- 

   int signo(int x) {
      if (x == 0) return  0;
      if (x <  0) return -1;
      if (x >  0) return  1;
   }

-}

-- |
-- >>> signo 6
-- 1
-- >>> signo 0
-- 0
-- >>> signo (-3)
-- -1

signo :: Integer -> Integer  -- predefinida como signum
signo x 
   | x == 0 = 0
   | x > 0 = 1
   | x < 0 = -1

-- Es mejor emplear guardas que anidar if then else
-- guarda ::= '|' exp_Bool = exp

maxOf' :: Integer -> Integer -> Integer
maxOf' x y
    | x >= y = x
    | otherwise = y

signo' :: Integer -> Integer
signo' x
   | x > 0 = 1
   | x < 0 = -1
   | otherwise = 0

-- Curiosidad: las guardas de una ecuación no tienen que ser exhaustivas;
-- las guardas de una función (que puede tener varias ecuaciones) sí deben
-- ser exhaustivas.
-- Utilizaremos este truco para implementar las semánticas.

múltiploDe2o5 x
   | x `mod` 2 == 0 = "múltiplo de 2"

múltiploDe2o5 x
   | x `mod` 5 == 0 = "múltiplo de 5"

múltiploDe2o5 x = "no es múltiplo ni de 2 ni de 5"

-- | 4. Tuplas
----------------------------------------------------------------

--       valor                    tipo
-- ('a', 1 <= 2, ord 'A') :: (Char, Bool, Int)

-- el número de componentes es fijo
-- cada componente puede ser de un tipo distinto
-- es un producto cartesiano
-- existe la tupla vacía: ()
-- no existe la tupla unitaria: (x) confusión si 
-- utilidad de las tuplas: una función puede devolver varios datos

predSuc :: Integer -> (Integer, Integer)
predSuc x = (x-1, x+1)

-- | 5. Parámetros formales y patrones
----------------------------------------------------------------

-- un parámetro formal es un patrón donde debe encajar el parámetro real

-- patrón variable: x
módulo :: (Integer, Integer) -> Integer
módulo x = square (fst x) + square (snd x)
-- variable x, es una tupla con dos componentes
-- fst x (primer componente de la tupla)
-- snd x (último componente de la tupla)

-- patrón tupla: (x,y)
módulo' :: (Integer, Integer) -> Integer
módulo' (x, y) = square x + square y
-- (x, y) patrón de tupla y sus componentes son a su vez patrones variables

-- Estilo Java con paso de patron de variable (variable tipo tupla)
esOrigen :: (Integer, Integer) -> Bool -- Si esta en el origen de coordenadas
esOrigen x = fst x == 0 && snd x == 0

-- patrón constante: 0
esOrigen' :: (Integer, Integer) -> Bool
esOrigen' (0, 0) = True
esOrigen' (x, y) = False

-- patrón subrayado: _
esOrigen'' :: (Integer, Integer) -> Bool
esOrigen'' (0, 0) = True
esOrigen'' _      = False 
-- Ej: esOrigen' (1-1, 0) -> true
-- patron _ vale para todo
-- Se prioriza el orden. La primera que te de un patrón que funcione se aplica. El resto se ignoran.
-- | 6. Polimorfismo
----------------------------------------------------------------

-- versiones monomórficas (tipos concretos): Integer, Bool, Char
-- Para los siguientes ejemplos es redundante, se puede reducir con las versiones polimórficas (tipo genérico)

primeroI :: (Integer, Integer) -> Integer
primeroI (x, _) = x

primeroC :: (Char, Bool) -> Char
primeroC (x, _) = x

primeroB :: (Bool, Integer) -> Bool
primeroB (x, _) = x

-- versiones polimórficas (variables de tipo, genericidad en Java)

primero :: (a, b) -> a -- predefinida como fst
primero (x, _) = x

segundo :: (a, b) -> b -- predefinida como snd
segundo (_, y) = y

-- | 7. Polimorfismo restringido o sobrecarga
----------------------------------------------------------------

-- 1) en Java el genérico <T> puede ser reemplazado por cualquier tipo (clase)
-- en Haskell una variable de tipo a puede ser reemplazada por cualquier tipo
-- 2) en Java puedo restringir un genérico: T extends Comparable<T>
-- en Haskell puedo restringir una variable de tipo: Ord a => a
-- 3) una clase Haskell es semejante (pero no equivalente) a una interfaz Java

-- la clase Eq
-- Eq a significa que a tiene restringida la igualdad, permite eso.

sonSimétricos :: Eq a => (a, a) -> (a, a) -> Bool
sonSimétricos (x, y) (u, v) =
       x == v && y == u -- Eq: permite usar la igualdad: ==, /=
-- :i Eq --> da mucha informacion sobre eso
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}

sonSimétricos' :: (Eq a, Eq b) => (a, b) -> (b, a) -> Bool
sonSimétricos' (x, y) (u, v) =
        x == v && y == u -- Eq: usamos igualdad: ==, /=

-- la clase Ord: indica que está ordenado
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}

estáOrdenada :: Ord a => (a, a) -> Bool
estáOrdenada (x, y) = x <= y -- Ord: usamos comparación: <, <=, >, >=, compare, ==, /=

-- | 8. Definiciones locales
----------------------------------------------------------------

-- definición local, función error y pereza

raíces :: Double -> Double -> Double -> (Double, Double)
raíces a b c
   | abs a <= epsilon = error "la ecuación no es de segundo grado"
   | disc < 0 = error "raíces complejas"
   | otherwise = ((-b + raízDisc) / dosA, (-b - raízDisc) / dosA )
   where
      epsilon = 1e-8
      disc = b**2 - 4*a*c
      raízDisc = sqrt disc -- raiz del discriminante --> disc
      dosA = 2*a

-- | 9. Listas y patrones
----------------------------------------------------------------
--  azúcar sintáctico --> [1, 2, 3] (es por legilibilidad)
--  otra forma --> 1 : 2 : 3 : []   (dos constructures)
-- La lista vacía tiene tipo polimorfico (a) pues da igual.
--        valor                           tipo (no se refleja el tamaño de la lista)
-- [1, -2,  3 + 5,  123 `div` 6]    :: [Integer]
-- 
-- 1 : -2 : 3 + 5 : 123 `div` 6: [] :: [Integer]
-- Cabeza : 1
-- Cola: el resto

-- número de componentes variable
-- todos los componentes deben ser del mismo tipo
-- existe la lista vacía: []
-- existe la lista unitaria: [x]
-- constructores de listas:
--      [] :: [a]
--      (:) :: a -> [a] -> [a]      (cabeza elemento tipo a, cola tipo lista [a], resultado tipo [a] pues es otra lista)
-- Un constructor también es un patrón.
-- funciones predefinidas:

{-
    head              tail
     |   ------------------------------
    [x1, x2, x3, ............, xn-1, xn]
     ------------------------------  |
                 init               last
-- init --> (prefijo de todos menos el último)  
    -------------- + -----------------
         take              drop

   null
   elem / notElem
   length
   (++)

-}

-- patrones habituales:
{-
     []               lista vacía
     [x]              lista unitaria
     (x:xs)           lista no vacía  (x -> cabeza, xs -> cola) la cola podría ser vacia
-}

-- regla general:  [...]   ==> longitud fija,
--               (...:...) ==> longitud mínima

{-

     [x,y,z]          lista con tres elementos
     (x:y:zs)         lista con al menos dos elementos
-}

longitud :: [a] -> Integer -- predefinida como length
longitud []     =  0
longitud (_:xs) =  1 + longitud xs

ordenada :: Ord a => [a] -> Bool
ordenada []       = True
ordenada [_]      = True  -- tiene un elemento solo
ordenada (x:y:ys) =  x <= y && ordenada (y:ys)

-- | 10. Orden superior
----------------------------------------------------------------

-- |
-- >>> twice (+1) 5
-- 7
--
-- >>> twice (*2) 3
-- 12

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- |
-- >>> mapTuple (+1) (*2) (3, 4)
-- (4,8)
--
-- >>> mapTuple ord (==5) ('A', 3 + 1)
-- (65,False)

-- f: a -> c, g: b -> d
-- f&g : (a, b) -> (c, d)
mapTuple :: (a->c) -> (b->d) -> (a,b) -> (c,d)
mapTuple f g (x,y) = (f x, g y)

-- map y secciones
-- Ponemos en la consola :t map
-- map :: (a -> b) -> [a] -> [b]
-- map (*2) [5, 7, 9, 11]
-- [10, 14, 18, 22]
-- |
-- >>> aprobadoGeneral [1..10]
-- [5.0,5.0,5.0,5.0,5.0,6.0,7.0,8.0,9.0,10.0]
--
-- >>> aprobadoGeneral [4.7, 2.5, 7, 10, 8.7]
-- [5.0,5.0,7.0,10.0,8.7]

aprobadoGeneral :: [Double] -> [Double]
aprobadoGeneral xs = map aprobar xs
   where aprobar x = max 5 x 
-- Otra forma: 
-- where aprobar = max 5    (simplifica y funciona igual)
-- Otra forma aun más simplificada
aprobadoGeneral' :: [Double] -> [Double]
aprobadoGeneral' xs = map (max 5) xs
-- aprobadoGeneral' xs = map (max 5)   (también funciona)

-- lambda expresiones: funciones anonimas (sin nombre)
-- Transformación de: -- f arg1 arg2 ... argn x = (exp) x       (x es patrón variable)
-- \ arg1, arg2 ... argn -> expr    (se escribe así)
-- :: arg1 -> arg2 -> ... -> argn -> exp  (mejor simmplifica lo de arriba)

-- Ej consola: map (\ x -> 2*x +5) [2, 6, 8, 12]
-- [9, 17, 21, 29]
-- MAD = Multiply, Add, Divide

-- |
-- >> f = mad 2 3 7
-- >> f 5
-- 6
-- >> f 10
-- 2
-- Modulo no es con %, es con `mod`
-- construimos una función MAD y la devolvemos como resultado
mad :: Int -> Int -> Int -> (Int -> Int)
mad m a d = \ x -> (m * x + a) `mod` d 
-- Recibe 3 parametros y devuelve una funcion
-- En consola: hash = mod 2 3 7
-- hash 5
-- 6
-- | 11. Plegado de listas
----------------------------------------------------------------

-- revisión de la recursión sobre listas

-- |
-- >>> suma [1..10]
-- 55
--
-- >>> suma [7]
-- 7
--
-- >>> suma []
-- 0

suma :: Num a => [a] -> a
suma []     = 0
suma (x:xs) = x + suma xs
-- Otra forma
-- suma :: Num a => [a] -> a
-- suma []     = 0
-- suma (x:xs) = (+) x (suma xs)


-- |
-- >>> conjunción [1 == 1, 'a' < 'b', null []]
-- True
--
-- >>> conjunción [1 == 1, 'a' < 'b', null [[]]]
-- False
--
-- >>> conjunción []
-- True

conjunción :: [Bool] -> Bool
conjunción []     = True
conjunción (x:xs) = x && conjunción xs
-- Otra forma:
-- conjunción :: [Bool] -> Bool
-- conjunción []     = True
-- conjunción (x:xs) = (&&) x (conjunción xs)
-- |
-- >>> esPalabra "haskell"
-- True
--
-- >>> esPalabra "haskell 2021"
-- False
--
-- >>> esPalabra "h"
-- True
--
-- >>> esPalabra ""
-- True

esPalabra :: String -> Bool
esPalabra []     = True
esPalabra (x:xs) = isLetter x && esPalabra xs
-- Otra forma
-- esPalabra :: String -> Bool
-- esPalabra []     = True
-- esPalabra (x:xs) = f x (esPalabra xs)
--    where f x ys = isLetter x && ys

-- Composicion: f o g
-- <- [] <- [] <- x
-- |
-- >>> todasMayúsculas "WHILE"
-- True
--
-- >>> todasMayúsculas "While"
-- False
--
-- >>> todasMayúsculas ""
-- True

todasMayúsculas :: String -> Bool
todasMayúsculas []     = True
todasMayúsculas (x:xs) = isUpper x && todasMayúsculas xs
-- Otra forma 
-- todasMayúsculas :: String -> Bool
-- todasMayúsculas []     = True
-- todasMayúsculas (x:xs) = f x todasMayúsculas xs
--    where f x ys = isUpper x && ys
-- |
-- >>> máximo "hola mundo"
-- 'u'
--
-- >>> máximo [7, -8, 56, 17, 34, 12]
-- 56
--
-- >>> máximo [-8]
-- -8

máximo :: Ord a => [a] -> a
máximo [x]    = x
máximo (x:xs) = max x (máximo xs)

-- |
-- >>> mínimoYmáximo "hola mundo"
-- (' ','u')
--
-- >>> mínimoYmáximo [7, -8, 56, 17, 34, 12]
-- (-8,56)
--
-- >>> mínimoYmáximo [1]
-- (1,1)

mínimoYmáximo :: Ord a => [a] -> (a,a)
mínimoYmáximo [x] = (x,x)
mínimoYmáximo (x:xs) = (min x u, max x v)
  where (u,v) = mínimoYmáximo xs
-- Otra forma:
-- mínimoYmáximo :: Ord a => [a] -> (a,a)
-- mínimoYmáximo [x] = (x,x)
-- mínimoYmáximo (x:xs) = f x (mínimoYmaximo xs)
--   where f x (u, v) = (min x u, max x v)
-- |
-- >>> aplana [[1,2], [3,4,5], [], [6]]
-- [1,2,3,4,5,6]
--
-- >>> aplana [[1,2]]
-- [1,2]
--
-- >>> aplana []
-- []

aplana :: [[a]] -> [a]
aplana []       = []
aplana (xs:xss) = xs ++ aplana xss
-- Otra forma
-- aplana :: [[a]] -> [a]
-- aplana []       = []
-- aplana (xs:xss) = (++) xs aplana xss

-- En casi todas las anteriores se usa la misma idea
-- deducir el patrón de foldr
plegar :: (a -> b -> b) -> b -> [a] -> b
plegar f solBase xs = recLista xs
   where 
      recLista [] = solBase
      recLista (x:xs) = f x (recLista xs)
--- ...

-- resolver las anteriores funciones con foldr

-- |
-- >>> sumaR [1..10]
-- 55
--
-- >>> sumaR [7]
-- 7
--
-- >>> sumaR []
-- 0

sumaR :: Num a => [a] -> a
sumaR x = foldr (+) 0 x
-- plegar == foldr
-- |
-- >>> longitudR "hola mundo"
-- 10
--
-- >>> longitudR [True]
-- 1
--
-- >>> longitudR []
-- 0

longitudR :: [a] -> Integer
longitudR x = foldr (\ _ solCola -> 1 + solCola) 0 x

-- |
-- >>> conjunciónR [1 == 1, 'a' < 'b', null []]
-- True
--
-- >>> conjunciónR [1 == 1, 'a' < 'b', null [[]]]
-- False
--
-- >>> conjunciónR []
-- True

conjunciónR :: [Bool] -> Bool
conjunciónR x = foldr (&&) True x

-- |
-- >>> esPalabraR "haskell"
-- True
--
-- >>> esPalabraR "haskell 2021"
-- False
--
-- >>> esPalabraR "h"
-- True
--
-- >>> esPalabraR ""
-- True

esPalabraR :: String -> Bool
esPalabraR = plegar (\cabeza solCola -> isLetter cabeza && solCola) True
-- |
-- >>> todasMayúsculasR "WHILE"
-- True
--
-- >>> todasMayúsculasR "While"
-- False
--
-- >>> todasMayúsculasR ""
-- True

todasMayúsculasR :: String -> Bool
todasMayúsculasR  x = plegar (\cabeza solCola -> isUpper cabeza && solCola) True x

-- |
-- >>> máximoR "hola mundo"
-- 'u'
--
-- >>> máximoR [7, -8, 56, 17, 34, 12]
-- 56
--
-- >>> máximoR [-8]
-- -8

máximoR :: Ord a => [a] -> a
máximoR x = foldr max (head x) x

-- |
-- >>> mínimoYmáximoR "hola mundo"
-- (' ','u')
--
-- >>> mínimoYmáximoR [7, -8, 56, 17, 34, 12]
-- (-8,56)
--
-- >>> mínimoYmáximoR [1]
-- (1,1)
-- No encaja en el plegado porque no encaja el caso base
mínimoYmáximoR :: Ord a => [a] -> (a,a)
mínimoYmáximoR x = foldr f (head x, head x) x
   where f cabeza (u, v) = (min cabeza u, max cabeza v)

-- |
-- >>> aplanaR [[1,2], [3,4,5], [], [6]]
-- [1,2,3,4,5,6]
--
-- >>> aplanaR [[1,2]]
-- [1,2]
--
-- >>> aplanaR []
-- []

aplanaR :: [[a]] -> [a]
aplanaR x = foldr (++) [] x

-- otros ejercicios de foldr

-- |
-- >>> mapR (2^) [0..10]
-- [1,2,4,8,16,32,64,128,256,512,1024]
--
-- >>> mapR undefined []
-- []
--
-- >>> mapR ord  "A"
-- [65]

mapR :: (a -> b) -> [a] -> [b]
mapR f x = foldr (\cabeza solCola -> f cabeza : solCola) [] x

-- |
-- >>> filterR even [1..20]
-- [2,4,6,8,10,12,14,16,18,20]
--
-- >>> filterR undefined []
-- []
--
-- >>> filterR even [5]
-- []

filterR :: (a -> Bool) -> [a] -> [a]
filterR f x = foldr g [] x 
   where g cabeza solCola = if f cabeza then cabeza : solCola 
                            else solCola

-- |
-- >>> apariciones 'a' "casa"
-- 2
-- >>> apariciones 'u' "casa"
-- 0

apariciones :: Eq a => a -> [a] -> Integer
apariciones x xs = foldr f 0 xs
   where f cabeza solCola = if x == cabeza then 1 + solCola
                            else solCola

-- |
-- >>> purgar "abracadabra"
-- "cdbra"
--
-- >>> purgar [1,2,3]
-- [1,2,3]
--
-- >>> purgar "aaaaaaaaaa"
-- "a"

purgar :: Eq a => [a] -> [a]
purgar x = foldr f [] x
   where f cabeza solCola = if elem cabeza solCola then solCola
                            else cabeza : solCola

-- |
-- >>> agrupa "mississippi"
-- ["m","i","ss","i","ss","i","pp","i"]
--
-- >>> agrupa [1,2,2,3,3,3,4,4,4,4]
-- [[1],[2,2],[3,3,3],[4,4,4,4]]
--
-- >>> agrupa []
-- []

agrupa :: Eq a => [a] -> [[a]]
agrupa x = foldr f [] x
   where f cabeza solCola = if null solCola then [[cabeza]]  -- solCola == null
                            else if cabeza == head (head solCola) then (cabeza : head solCola) : tail solCola -- si es igual, lo metemos en la lista
                            else [cabeza] : solCola -- si no es igual, concatenamos con otra lista

-- | 12. Tipos algebraicos: recursión y plegados
----------------------------------------------------------------

data Tree a = Empty
            | Leaf a
            | Node a (Tree a) (Tree a)
            deriving Show

treeI :: Tree Integer
treeI = Node 1
             (Node 2 (Leaf 4) (Leaf 5))
             (Node 3 Empty (Leaf 6))

treeC :: Tree Char
treeC = Node 'z'
          (Node 't' (Node 's' Empty (Leaf 'a')) (Leaf 'g'))
          (Node 'w' (Leaf 'h') (Node 'p' (Leaf 'f') (Leaf 'n')))

-- |
-- >>> treeSize treeI
-- 6
--
-- >>> treeSize treeC
-- 10

treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Leaf _) = 1
treeSize (Node _ l r) = 1 + treeSize l + treeSize r
-- Otra forma:
-- treeSize :: Tree a -> Integer
-- treeSize Empty = 0
-- treeSize (Leaf x) = (const 1) x
-- treeSize (Node _ l r) = 1 + treeSize l + treeSize r

-- |
-- >>> treeHeight treeI
-- 3
-- >>> treeHeight treeC
-- 4

treeHeight :: Tree a -> Integer
treeHeight Empty = 0
treeHeight (Leaf _) = 1
treeHeight (Node _ l r) = 1+ max (treeHeight l) (treeHeight r)
-- Otra forma:
-- treeHeight :: Tree a -> Integer
-- treeHeight Empty = 0
-- treeHeight (Leaf _) = (\ n -> 1) x     tambien puedes poner: treeHeight (Leaf _) = (const 1) x
-- treeHeight (Node _ l r) = 1+ max (treeHeight l) (treeHeight r)
-- |
-- >>> treeSum treeI
-- 21

treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Leaf x) = x
treeSum (Node x l r) = x + treeSum l + treeSum r
-- Otra forma: 
-- treeSum :: Num a => Tree a -> a
-- treeSum Empty = 0
-- treeSum (Leaf x) = (\ n -> n) x        tambien puedes poner:  treeSum (Leaf x) =  id x
-- treeSum (Node x l r) = x + treeSum l + treeSum r

-- |
-- >>> treeProduct treeI
-- 720

treeProduct :: Num a => Tree a -> a
treeProduct Empty = 1
treeProduct (Leaf x) = x
treeProduct (Node x l r) = x * treeProduct l * treeProduct r
-- Otra forma:
-- treeProduct :: Num a => Tree a -> a
-- treeProduct Empty = 1
-- treeProduct (Leaf x) = (\ n -> n) x        tambien puedes poner:  treeProduct (Leaf x) =  id x
-- treeProduct (Node x l r) = x * treeProduct l * treeProduct r

-- |
-- >>> treeElem 5 treeI
-- True
--
-- >>> treeElem 48 treeI
-- False
--
-- >> treeElem 'w' treeC
-- True
--
-- >>> treeElem '*' treeC
-- False
-- Pertenece el elemento al arbol??
treeElem :: Eq a => a -> Tree a -> Bool
treeElem _ Empty = False
treeElem x (Leaf y) = x == y  -- TRUE
treeElem x (Node y l r) = x == y || treeElem x l || treeElem x r
-- Otra forma:
-- treeElem :: Eq a => a -> Tree a -> Bool
-- treeElem _ Empty = False
-- treeElem x (Leaf y) = (\ n -> n == y) x          tambien se puede poner: treeElem x (Leaf y) = (== y) x
-- treeElem x (Node y l r) = x == y || treeElem x l || treeElem x r
-- |
-- >>> treeToList treeI
-- [4,2,5,1,3,6]
--
-- >>> treeToList treeC
-- "satgzhwfpn"

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Leaf x) = [x]        -- (\ n -> [n]) x          tambien se puede poner: treeToList (Leaf x) = (:[]) x
treeToList (Node x l r) = x : treeToList l ++ treeToList r

-- |
-- >>> treeBorder treeI
-- [4,5,6]
--
-- >>> treeBorder treeC
-- "aghfn"

treeBorder :: Tree a -> [a]
treeBorder Empty = []
treeBorder (Leaf x) = [x]
treeBorder (Node x l r) = treeBorder l ++ treeBorder r

-- introducir el plegado del tipo Tree a
-- recTree :: (a -> b -> b -> b) -> (a -> b) -> b -> Tree a -> b
-- recTree Empty = solBase       (caso base )
-- recTree (Leaf x) = fl x          (funcion aplicada a x)
-- recTree (Node x l r) = fn x (recTree l) (recTree r)

-- treeFold :: (a -> b -> b) -> b -> Tree a -> b
-- treeFold f acc tree = plegar f acc (inorder tree)
--   where
--     inorder Leaf = []
--     inorder (Node left x right) = inorder left ++ [x] ++ inorder right

-- resolver los ejercicios anteriores con foldTree
foldTree :: (a  -> b -> b -> b) -> (a -> b) -> b -> Tree a -> b 
foldTree fn fl solEmpty t = plegar t
   where
      plegar Empty = solEmpty
      plegar (Leaf x) = fl x
      plegar (Node x l r) = fn x (plegar l) (plegar r)
-- |
-- >>> treeSize' treeI
-- 6
--
-- >>> treeSize' treeC
-- 10

treeSize' :: Tree a -> Integer
treeSize' Empty = 0
treeSize' (Leaf x) = (const 1) x
treeSize' (Node x l r) = f x (treeSize' l) (treeSize' r)
   where f _ sl sr = 1 + sl + sr

-- |
-- >>> treeHeight' treeI
-- 3
-- >>> treeHeight' treeC
-- 4

treeHeight' :: Tree a -> Integer
treeHeight' Empty = 0
treeHeight' (Leaf x) = (const 1) x
treeHeight' (Node x l r) = f x (treeHeight' l) (treeHeight' r)
   where f _ sl sr = 1 + max sl sr

-- |
-- >>> treeSum' treeI
-- 21

treeSum' :: Num a => Tree a -> a
treeSum' Empty = 0
treeSum' (Leaf x) = x
treeSum' (Node x l r) = f x (treeSum' l) (treeSum' r)
   where f x sl sr = x + sl + sr

-- |
-- >>> treeProduct' treeI
-- 720

treeProduct' :: Num a => Tree a -> a
treeProduct' Empty = 1
treeProduct' (Leaf x) = x
treeProduct' (Node x l r) = f x (treeProduct' l) (treeProduct' r)
   where f x sl sr = x * sl * sr

-- |
-- >>> treeElem' 5 treeI
-- True
--
-- >>> treeElem' 48 treeI
-- False
--
-- >> treeElem' 'w' treeC
-- True
--
-- >>> treeElem' '*' treeC
-- False

treeElem' :: Eq a => a -> Tree a -> Bool
treeElem' x t = foldTree (\ n sl sr -> x == n || sl || sr) (==x) False t
-- -- Otra forma:
-- treeElem' :: Eq a => a -> Tree a -> Bool
-- treeElem' _ Empty = False
-- treeElem' x (Leaf y) = x == y        -- puedo poner (== y) x
-- treeElem' x (Node y l r) = f y (treeElem' x l) (treeElem' x r)
--    where f y sl sr = x == y || sl || sr
-- |
-- >>> treeToList' treeI
-- [4,2,5,1,3,6]
--
-- >>> treeToList' treeC
-- "satgzhwfpn"

treeToList' :: Tree a -> [a]
treeToList' t = foldTree (\ x sl sr -> sl ++ x : sr) (:[]) [] t

-- Otra forma:
-- treeToList' :: Tree a -> [a]
-- treeToList' Empty = []
-- treeToList' (Leaf x) = (const [x]) x
-- treeToList' (Node x l r) = f x (treeToList' l) (treeToList' r)
--    where f x sl sr = x : sl ++ sr

-- -- |
-- >>> treeBorder' treeI
-- [4,5,6]
--
-- >>> treeBorder' treeC
-- "aghfn"
treeBorder' :: Tree a -> [a]
treeBorder' = foldTree (\ _ sl sr -> sl ++ sr) (:[]) [] 
-- Otra forma:
-- treeBorder' :: Tree a -> [a]
-- treeBorder' Empty = []
-- treeBorder' (Leaf x) = [x]
-- treeBorder' (Node x l r) = f x (treeBorder' l) (treeBorder' r)
--    where f x sl sr = if null sl && null sr then [x]   -- nodo hoja
--                      else sl ++ sr        -- nodo interno

