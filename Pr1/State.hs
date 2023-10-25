-- State.hs - Implementation of State

module State where

type  Var    =  String
type  Z      =  Integer
type  State  =  Var -> Z

sInit :: State
sInit "x" =  3
sInit _   =  0
