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
    let startSpace = adjustMap (tripleCartesian [-1,0, 1] [-1, 0 .. length lines + 1] [-1, 0 .. length (head lines) + 1]) $ getStartingMap lines
    print startSpace
    hClose handle

getStartingMap :: [String] -> Map.Map (Int, Int, Int) Bool
getStartingMap input = Map.fromList $ getTriple [0 .. length input] [0 .. length (head input)] input

getTriple ::  [Int] -> [Int] -> [String] -> [((Int,Int,Int), Bool)]
getTriple rows cols input = concat [[((0, row, col), char == '#') | (col,char)<- tuples] | (row, tuples)<-zip rows (map (zip cols) input)]

tripleCartesian ints1 ints2 ints3 = trace "Cartesien" (concat [ [(z, y, x) | (y, x) <- tuples] | (z, tuples) <- zip ints1 (map (\y -> [(y, x) | x <- ints3]) ints2)])

adjustMap :: [(Int,Int,Int)] -> Map.Map (Int,Int,Int) Bool -> Map.Map (Int,Int,Int) Bool
adjustMap [] set = set
adjustMap (tr:triples) chart = if isNothing value then adjustMap triples (Map.insert tr False chart) else adjustMap triples chart
    where value = chart Map.!? tr