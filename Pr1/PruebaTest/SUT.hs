{-|
Module      : SUT.hs
Description : Simple example of unit testing with HUnit.
Copyright   : (c) Pablo López, 2020

The function 'suma' is the Subject Under Test (SUT).
The unit tests reside in the SUTTest.hs module.
-}

module SUT (suma) where

suma :: Num a => [a] -> a
suma []     = 0
suma (x:xs) = x +  suma xs
