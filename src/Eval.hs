module Eval
    ( eval
    , evalS
    , evalLoop
    , evalLoopS
    , ioEvalLoop
    ) where

import Syntax
import Parse
import Alpha
import Beta

eval :: [Exp] -> [Exp]
eval [] = []
eval (Lambda c (Body bs) : []) = (Lambda c (Body bs) : [])
eval (Lambda c (Body bs):x:xs) = β c x bs ++ eval xs
eval (Paren ps : xs) = Paren (eval ps) : eval xs
eval (Body  bs : xs) = Body  (eval bs) : eval xs
eval (Var x : xs) = Var x : eval xs

evalS :: String -> [Exp]
evalS s =
    let Paren xs = parse s
     in eval xs

evalLoop :: [Exp] -> [Exp]
evalLoop xs =
    if ys /= xs
    then evalLoop ys
    else ys
      where ys = eval xs

evalLoopS :: String -> [Exp]
evalLoopS s = 
    let Paren xs = parse s
     in evalLoop xs

evalNext :: [Exp] -> [Exp]
evalNext [] = []
evalNext (x:xs) =
    if ys /= xs
    then x : ys
    else x : evalNext xs
    where ys = eval xs

ioEvalLoop' :: Int -> [Exp] -> IO()
ioEvalLoop' i xs = do
    putStrLn $ show i ++ ". " ++ unparse' xs
    let ys = eval xs
    if ys /= xs
    then ioEvalLoop' (i+1) ys
    else do
        let xs' = evalNext xs
         in if xs' /= xs
            then ioEvalLoop' (i+1) xs'
            else putStrLn ""

ioEvalLoop = ioEvalLoop' 0
