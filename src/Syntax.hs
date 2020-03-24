module Syntax where

data Exp = Paren [Exp]
         | Lambda Char Exp
         | Var Char
         | Body [Exp]
         deriving (Show, Eq)
