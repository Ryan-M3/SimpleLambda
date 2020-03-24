module Beta (β, beta) where

import Syntax

-- |Beta reduction is essentially the concepty of passing
-- an argument into a function.
β :: Char -> Exp -> [Exp] -> [Exp]
β _ _ [] = []
β parameter replacement xs = β' parameter replacement <$> xs
    where β' :: Char -> Exp -> Exp -> Exp
          β' parameter replacement x =
              let self = β' parameter replacement
               in case x of
                    Paren ps -> Paren (self <$> ps)
                    Body  bs -> Body  (self <$> bs)
                    Lambda c (Body bs) -> Lambda c (Body $ self <$> bs)
                    Var c -> if parameter == c
                             then replacement
                             else x
beta = β
