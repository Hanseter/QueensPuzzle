module Main where

import System.Environment
import Data.List
import QueensPuzzle
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "How many queens?"
    tmp <- getLine
    let n = read tmp
    showSolutions $ findPlacements n

showSolutions :: [[Integer]] -> IO ()
showSolutions xs 
        | null xs = return ()
        | otherwise = do
    putStrLn "How many results do you want to see?"
    tmp <- getLine
    let n = read tmp 
    print $ take n xs
    showSolutions $ drop n xs
