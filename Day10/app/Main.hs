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
    let temp =  Map.fromList  (zip appNumbers (repeat 0 ) )
    let dyn =  Map.insert (last appNumbers) 1 temp
    print ( getCombinations (reverse appNumbers) [] dyn)
    hClose handle

count1 :: [Int] -> Int
count1 [a,b] = if (b-a) == 1 then 1 else 0
count1 (a:b:ls) = if (b-a) == 1 then 1 + (count1 (b:ls)) else count1 (b:ls)

count3 :: [Int] -> Int
count3 [a,b] = if (b-a) == 3 then 1 else 0
count3 (a:b:ls) = if (b-a) == 3 then 1 + (count3 (b:ls)) else count3 (b:ls)

getCombinations :: [Int] -> [Int]-> Map.Map Int Int -> Int
getCombinations [0] [] dyn = fromMaybe (-1)  first
    where first = dyn Map.!? 0
getCombinations (a:rev) [] dyn = if length compatibles /= 0 then getCombinations (a:rev) compatibles dyn else getCombinations rev compatibles dyn
    where compatibles = getCompatibles a rev
getCombinations (a:rev) (s:stack) dyn = getCombinations (a:rev) stack $ Map.insert s ((fromMaybe 0 $ dyn Map.!? s) +  (fromMaybe 0 $ dyn Map.!? a)) dyn

getCompatibles a (b:rev) = if b + 3 >= a then b: getCompatibles a rev else []