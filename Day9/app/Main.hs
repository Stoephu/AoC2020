module Main where

import System.IO
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.Text(pack, unpack, replace)
import Debug.Trace

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let lines = splitOn "\n"  contents
    let numbers = map read lines
    let wrong = searchwrong numbers
    print wrong
    let snake = findsnake wrong 1 numbers
    let min = minimum snake
    let max = maximum snake
    print min
    print max
    print (max + min)
    hClose handle

cartProd xs ys = [[x,y] | x <- xs, y <- ys, y /= x]
sums (l:ls) = map sum ls
searchwrong ls = if (last current) `elem` (sums(cartProd (init current) (init current))) then searchwrong (tail ls) else last current
    where current = take 26 ls


findsnake :: Int -> Int -> [Int] -> [Int]
findsnake goal len ls 
    | current == goal = take len ls
    | current > goal = findsnake goal (len-1) (tail ls)
    | current < goal = findsnake goal (len+1) ls
        where current = sum (take len ls)