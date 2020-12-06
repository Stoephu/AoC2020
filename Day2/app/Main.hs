module Main where

import Lib
import Data.List.Split
import System.IO

-- password list [min max character "" pw]

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle  
    let splits = splitOn "\n" contents
    let lines = init splits
    let preppedstring = map prepstring lines
    let splitted = map (splitOn " " ) preppedstring
    -- Part 1
    let checked = map validatePassword splitted
    let nTrue = foldr (\bo -> if bo then (+1) else (+0)) 0 checked
    print nTrue
    -- Part 2
    let checked2 = map validatePassword2 splitted
    let trues = filter (== True) checked2
    let nTrue2 = length trues
    --let nTrue2 = length . (filter (==True)) checked2
    print nTrue2
    hClose handle

validatePassword2 :: [[Char]] -> Bool
validatePassword2 (pos1:pos2:ch:_:pw:_) = 1 == amountofhits 1 (read pos1) (read pos2) (head ch) pw

amountofhits :: Int -> Int -> Int -> Char -> [Char] -> Int
amountofhits _ _ _ _ [] = 0
amountofhits i p1 p2 ch (x:xs) = if (i == p1 || i == p2) && x == ch then 1 + amountofhits (i+1) p1 p2 ch xs else amountofhits (i+1) p1 p2 ch xs

prepstring string = map repl string

validatePassword :: [[Char]] -> Bool
validatePassword (min:max:ch:_:pw:_) =  
    if nOccurences >= read min && nOccurences <= read max 
    then True 
    else False
    where nOccurences = count ch pw
count :: [Char] -> [Char] -> Int
count ch string = length reduced
    where reduced = filter (==(head ch)) string
repl ':' = ' '
repl '-' = ' '
repl  c   = c

flatten [] = []
flatten (ls:ls') = ls ++ flatten ls'