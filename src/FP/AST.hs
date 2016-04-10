module FP.AST
    ( Function(..)
    , Definition(..)
    , Expression(..)
    , Program(..)
    ) where

import FP.Value

data Function = Function Symbol
              | Composition Function Function
              | Construction [Function]
              | Condition Function Function Function
              | Constant Object
              | Insert Function
              | ApplyToAll Function
              | BinaryToUnary Function Object
              | While Function Function
              deriving (Show, Eq)

data Definition = Definition Symbol Function
                deriving (Show, Eq)

data Expression = Object Object
                | Application Function Object
                deriving (Show, Eq)

data Program = Program [Definition] Expression
             deriving (Show, Eq)
