module Main where

import Lib
import System.IO
import Data.List.Split
import qualified Data.Set as Set

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let paragraphs = splitOn "\n\n"  contents
    let splittedparas = map ( splitOn "\n") paragraphs
    let sets = map getSet splittedparas
    let sizes = map Set.size sets
    print (sum sizes)
    -- part 2
    let intersects = map getIntersectionSet splittedparas
    let intersectSizes = map Set.size intersects
    print ( sum intersectSizes)
    hClose handle

getSet [string] = Set.fromList string
getSet (string:strings) = Set.union (Set.fromList string) (getSet strings) 


getIntersectionSet [string] = Set.fromList string
getIntersectionSet (string:strings) = Set.intersection (Set.fromList string) (getIntersectionSet strings)