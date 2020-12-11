module Main where
import System.IO
import Data.List.Split
import Data.List
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
    let numbers = sort (map read lines)
    let appNumbers = 0:numbers ++ [(maximum numbers) + 3]
    print appNumbers
    let ones = count1 appNumbers 
    let threes = count3 appNumbers
    print ones 
    print threes
    print (ones * threes)
    hClose handle

count1 :: [Int] -> Int
count1 [a,b] = if (b-a) == 1 then 1 else 0
count1 (a:b:ls) = if (b-a) == 1 then 1 + (count1 (b:ls)) else count1 (b:ls)

count3 :: [Int] -> Int
count3 [a,b] = if (b-a) == 3 then 1 else 0
count3 (a:b:ls) = if (b-a) == 3 then 1 + (count3 (b:ls)) else count3 (b:ls)

getCombinations ls = 

getNumberCompatibles x (l:ls)
    | x >= l = 1+ getCompatibles (x:ls)
    | otherwise = 0