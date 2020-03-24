module Parse
    ( parse
    , parse'
    , unparse
    , unparse'
    ) where

import Syntax
import Alpha

-- |Given an expression, return the equivalent string. This
-- differs from "show" in that show prints the Algebraic
-- Data Type representation, the actual parse tree. That
-- happens to be really useful for debugging, so a second
-- version is used: unparse would return "(λx.x)", whereas
-- show would return "Paren [Lambda 'x' (Body 'x')]".
unparse :: Exp -> String
unparse (Paren xs)    = "(" ++ concat (unparse <$> xs) ++ ")"
unparse (Lambda c xs) = ['λ', c, '.'] ++ unparse xs
unparse (Var c)       = [c]
unparse (Body xs)     = concat (unparse <$> xs)

-- |Variation on unparse that accepts a list of expressions
-- instead of a single expression.
unparse' :: [Exp] -> String
unparse' [] = ""
unparse' (x:xs) = "(" ++ unparse x ++ ")" ++ unparse' xs


-- | Create a list such that every item in the list is a string with an
-- equal number of open parenthesis and close parenthesis.
scope = scope' 0 []

scope' :: Int -> String -> String -> [String]
scope' 0 acc ('(':xs) = scope' 1 acc xs
scope' n acc ('(':xs) = scope' (n+1) (acc ++ "(") xs
scope' 1 acc (')':xs) = acc : scope' 0 [] xs
scope' n acc (')':xs) = scope' (n-1) (acc ++ ")") xs
scope' n acc (x:xs)   = scope' n (acc ++ [x]) xs
scope' _ [] [ ]       = []
scope' _ acc []       = acc:[]

-- Used in conjuction with parse.
parseNonterminal :: String -> [Exp]
parseNonterminal (' ':cs) = parseNonterminal cs
parseNonterminal [] = []
parseNonterminal ('λ':c:'.':cs) = [Lambda c (Body $ parseNonterminal cs)]
parseNonterminal (c:cs)         = Var c : parseNonterminal cs

-- |Given an input string, return an expression, the parse
-- tree equivalent of that string.
parse :: String -> Exp
parse (' ':cs)       = parse cs
parse (c:[])         = Var c
parse ('λ':c:'.':cs) = Lambda c (Body $ parseNonterminal cs)
parse s@('(':cs)     = Paren (parse <$> (scope s))
parse s              = Paren $ parseNonterminal s

-- |Variation of parse that returns a list of expressions
-- instead of wrapping everything in a parenthesis.
parse' :: String -> [Exp]
parse' s = let (Paren xs) = parse . α $ s in xs
