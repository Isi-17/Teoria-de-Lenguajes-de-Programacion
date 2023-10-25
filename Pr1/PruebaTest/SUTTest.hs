{-|
Module      : SUTTest.hs
Description : Simple example of unit testing with HUnit.
Copyright   : (c) Pablo López, 2020

The function 'suma' is imported from SUT.hs (Subject Under Test)
-}

module SUTTest where

import           SUT        (suma) -- se importa lo que se va a probar
import           Test.HUnit

-- | simple examples (see slides)

-- | HUnit assertEquals
-- (~=?) :: (Show a, Eq a) => a -> a -> Test

-- Para ejecutarlo en ghci:
-- > runTestTT testSimple
testSimple :: Test
testSimple = 55 ~=? suma [1..10]
-- Cases: 1 Tried: 1 Errors: 0 Failures: 0
-- 'Error' es un error de ejecución, 'Failure' es un error de prueba
-- Ejemplo de 'Error': si ponemos (55 `div` 0)

testSimpleConMensaje :: Test
testSimpleConMensaje = "suma [1..10] /= 55" ~: 55 ~=? suma [1..10]

testBooleano :: Test
testBooleano = 1 <= 2 ~? "1 debe ser menor o igual que 2"

listaTests :: Test
listaTests =
  TestList [ testSimple
           , testSimpleConMensaje
           , testBooleano
           ]

agrupaTests :: Test
agrupaTests =
   test [ testSimple
        , testSimpleConMensaje
        , testBooleano
        ]

-- | unit testing for 'suma'

testSumaNull :: Test
testSumaNull = "sum null list" ~: 0 ~=? suma []

testSumaSingleton :: Test
testSumaSingleton = "sum singleton list"  ~: 1 ~=? suma [1]

testSumaTwoElements :: Test
testSumaTwoElements = suma [1,2] /= 4 ~? "assertNotEqual is not defined" -- assertBool

testSumaThreeElements :: Test
testSumaThreeElements = "sum list with 3 elements" ~:  6 ~=? suma [1,2,3]

-- | group tests for 'suma'

testsSuma :: Test
testsSuma = test [ testSumaNull,
                   testSumaSingleton,
                   testSumaTwoElements,
                   testSumaThreeElements]

-- | run a particular test

runTestSumaSingleton :: IO Counts
runTestSumaSingleton = runTestTT testSumaSingleton

-- | run tests for 'suma'

runTestsSuma :: IO Counts
runTestsSuma = runTestTT testsSuma

-- | use a 'main' function in the testing unit

main :: IO ()
main = do
         testReport <- runTestsSuma
         print $ show testReport


-- Escribir por consola:
-- cabal update
-- cabal install HUnit

-- ghci
-- immport Test.HUnit


