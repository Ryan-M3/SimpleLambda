module Main where

import System.IO
import System.Environment
import Lib

main :: IO ()
main = do
    [fname] <- getArgs
    handle  <- openFile fname ReadMode
    hGetContents handle >>= ioEvalLoop . parse'
    hClose handle
