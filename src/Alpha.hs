module Alpha
    ( α, alpha
    , η, etaEquivalent
    , simplify
    ) where

import Data.HashMap
import Data.Char
import Syntax

-- |Tracker is a data structure used to rename variables in
-- lambda expressions in order to avoid name collisions.
data Tracker = Tracker [Map Char Int] Int (Map Int Char)

newTracker = Tracker [fromList []] 0 (fromList [])

-- |Call this when an open-parenthesis is encountered.
addScope :: Tracker -> Tracker
addScope (Tracker map maxID pam) =
    Tracker (fromList [] : map) maxID pam

-- |Call this when a close-parenthesis is encountered.
closeScope :: Tracker -> Tracker
closeScope (Tracker (m:ms) maxID pam) =
    Tracker ms maxID pam

-- |Add a variable to the data structure, assigning it an ID.
addVar :: Tracker -> Char -> Tracker
addVar (Tracker (m:map) maxID pam) char =
    Tracker (insert char maxID m : map) maxID' (insert maxID' char pam)
        where maxID' = maxID + 1

-- |Get the ID of a previously added variable.
getID :: Tracker -> Char -> Maybe Int
getID (Tracker [] _ _) char = Nothing
getID (Tracker hmaps _ _) char = getID' hmaps char
  where getID' [    ] char =  Nothing
        getID' (m:ms) char = 
          case Data.HashMap.lookup char m of
              Nothing   -> getID' ms char
              something -> something

-- |Using the tracker usually means we want to translate
-- one variable into another one. Typically if it's not
-- already in the tracker, we want to add it. This takes
-- takes care of both steps. Given a character, c, return
-- the equivalent and, if necessary, update the tracker
-- to include a new entry.
getOrAddVar :: Tracker -> Char -> (Tracker, Char)
getOrAddVar tracker@(Tracker _ maxID _) char =
    case getID tracker char of
      Nothing -> (addVar tracker char, id2var (maxID + 1))
      Just i  -> (tracker, id2var i)


-- |Given that all instances of Char, c, were replaced with
-- an Int, charID, restoreVar takes a charID and returns the
-- original c.
restoreVar :: Tracker -> Int -> Maybe Char
restoreVar (Tracker _ _ pam) charID = Data.HashMap.lookup charID pam

-- |A list of characters to use and in what order.
-- In other words, an id of 0 is x, 1 is y, etc.
printables = "xyzsquvwtrpabcdefghijklmnoABCDEFGHIJKLMNOPQRSTUVWXYZαβξδεφγθικμπψστυωηζΞΔΦΓΘΛΠΨΣΥΩζбБцЦДИвыЗЋЉЖ"

-- |Convert an ID to its visual representation.
id2var :: Int -> Char
id2var i = if   length printables <= i
           then chr (ord 'z' + i)
           else printables!!i

α' :: Tracker -> String -> String
α' tracker [] = ""
α' tracker ('(':cs) = '(' : α' (addScope tracker) cs
α' tracker ('λ':cs) = 'λ' : α' tracker cs
α' tracker ('.':cs) = '.' : α' tracker cs
α' tracker (')':cs) = ')' : α' (closeScope tracker) cs
α' tracker ( c :cs) =
    case getID tracker c of
      Nothing -> let tracker' = addVar tracker c
                  in case getID tracker' c of
                       Nothing -> error "tracker data structure error" 
                       Just i  -> id2var i : α' tracker' cs
      Just i  -> id2var i : α' tracker cs

-- |Rename all the characters in a valid lambda
-- expression(s) so that thre will not be naming
-- colllisions.
α :: String -> String
α = α' newTracker

alpha = α

simplify :: Exp -> Exp
simplify (Paren [x]) = simplify x
simplify (Body  [x]) = simplify x

-- Eta is essentially similar to alpha reduction in that it
-- relies on renaming the variables in the expression to a
-- single, canonical form. This will allow us to directly
-- compare two expresssions for equality.
η :: Tracker -> Exp -> (Tracker, Exp)
η tracker expr =
    case expr of
        Var c -> (tracker', Var c')
            where (tracker', c') = getOrAddVar tracker c
        Lambda c x ->
            let (tracker' , c') = getOrAddVar tracker c
                (tracker'', x') = η tracker' x
             in (tracker'', Lambda c' x')
        Paren [] -> (tracker, Paren [])
        Paren (x:[]) ->
            let (tracker', x') = η tracker x
             in (tracker', Paren [x'])
        Paren (x:xs) ->
            let (tracker', Paren xs') = η tracker (Paren xs)
                (tracker'', x') = η tracker' x
             in (tracker'', Paren (x':xs'))
        Body [] -> (tracker, Body [])
        Body (x:[]) ->
            let (tracker', x') = η tracker x
             in (tracker', Body [x'])
        Body (x:xs) ->
            let (tracker', Body xs') = η tracker (Body xs)
                (tracker'', x') = η tracker' x
             in (tracker'', Body (x':xs'))

etaEquivalent :: Exp -> Exp -> Bool
etaEquivalent a b = etafied a == etafied b
    where etafied = snd . η newTracker . simplify
