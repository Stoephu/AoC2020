module Main where

import Lib
import Data.List.Split
import System.IO
import Debug.Trace

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle  
    let splits = splitOn "\n" contents
    let lines = if last splits == "" then init splits else splits
    print lines
    let x_max = length (head lines)
    let y_max = length lines -1
    print (x_max,y_max)
    let x_slope = 3
    let y_slope = 1
    let treelines = [[c=='#' | c <- line] | line <- lines]
    let treesHit slope_x slope_y = map (`isTree` treelines) [(x `mod` x_max,y) | (x,y) <- positions slope_x slope_y y_max ] 
    print (positions x_slope y_slope y_max)
    print (treesHit x_slope y_slope) 
    let ntreeshit = length. filter (==True) $ treesHit x_slope y_slope 
    print ntreeshit
    -- part 2
    let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    let treefunctions = zip (map (treesHit) [x | (x,y) <- slopes]) [y | (x,y) <- slopes]
    let pos = positions 1 2 y_max
    --print pos
    let trees = [f y | (f,y) <- treefunctions]
    let filtered = map  (filter (==True))  trees
    let lengths = map length filtered
    --print lengths
    let result = product lengths
    print result
    hClose handle


isTree :: (Int,Int) -> [[Bool]] -> Bool
isTree (x,y) [] = error ("Empty List at " ++ show (x,y))
isTree (0,0) ls =  head . head $ ls
isTree (x,0) (l:ls) = isTree (x-1, 0) (tail l : ls)
isTree (x,y) (l:ls) = isTree (x, y-1) ls

positions slope_x slope_y y_max = zip ( map (slope_x*) [0,1.. length [0, slope_y ..  y_max]] )  [0, slope_y .. y_max] 