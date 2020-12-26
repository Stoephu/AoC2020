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
    let startSpace = getStartingMap lines
    --print(tripleCartesian [-1,0, 1] [-1, 0 .. length lines + 1] [-1, 0 .. length (head lines) + 1])
    --print startSpace
    let endSpace = simulate 6 startSpace [(0,0), (0,length lines), (0, length (head lines))]
    print endSpace
    print ( Map.size $ Map.filterWithKey f endSpace)

    let startSpace4 = getStartingMap4 lines
    --print(tripleCartesian [-1,0, 1] [-1, 0 .. length lines + 1] [-1, 0 .. length (head lines) + 1])
    --print startSpace
    let endSpace4 = simulate4 6 startSpace4 [(0,0),(0,0), (0,length lines), (0, length (head lines))]
    print endSpace4
    print ( Map.size $ Map.filterWithKey f endSpace4)
    hClose handle

f key value = value

getStartingMap :: [String] -> Map.Map (Int, Int, Int) Bool
getStartingMap input = Map.fromList $ getTriple [0 .. length input] [0 .. length (head input)] input

getTriple ::  [Int] -> [Int] -> [String] -> [((Int,Int,Int), Bool)]
getTriple rows cols input = concat [[((0, row, col), char == '#') | (col,char)<- tuples] | (row, tuples)<-zip rows (map (zip cols) input)]

tripleCartesian :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
tripleCartesian zs ys xs = concat [concat [[(z,y,x) | x <- xs] | y <- ys] | z <- zs]

adjustMap :: [(Int,Int,Int)] -> Map.Map (Int,Int,Int) Bool -> Map.Map (Int,Int,Int) Bool
adjustMap [] set = set
adjustMap (tr:triples) chart = if isNothing value then adjustMap triples (Map.insert tr False chart) else adjustMap triples chart
    where value = chart Map.!? tr

simulate :: Int -> Map.Map (Int,Int,Int) Bool -> [(Int,Int)] -> Map.Map (Int,Int,Int) Bool
simulate 0 space _ = space
simulate steps space [z,y,x] = trace (show x) simulate (steps-1) (calcStep (adjustMap (tripleCartesian [(fst z) -1 .. (snd z) + 1] [(fst y) -1 .. (snd y) +1] [(fst x) -1  .. (snd x) + 1]) space)) [newZ, newY, newX]
    where newZ = (fst z -1, snd z + 1)
          newY = (fst y -1, snd y + 1)
          newX = (fst x -1, snd x + 1)

calcStep :: Map.Map (Int,Int,Int) Bool -> Map.Map (Int,Int,Int) Bool
calcStep space = Map.mapWithKey (modify space) space

modify :: Map.Map (Int,Int,Int) Bool -> (Int,Int,Int) -> Bool -> Bool
modify space (z,y,x) value = applyRule (nNeighbors state (tripleCartesian (ext z) (ext y) (ext x)) space) state
    where ext n = [n-1, n, n+1]
          state = fromMaybe False (space Map.!? (z,y,x))

nNeighbors :: Bool -> [(Int,Int,Int)] -> Map.Map (Int,Int,Int) Bool -> Int
nNeighbors _ [] space = 0
nNeighbors True coords space = nNeighbors False coords space - 1 
nNeighbors False (c:coords) space = if fromMaybe False (space Map.!? c) then 1 + nNeighbors False coords space else nNeighbors False coords space

applyRule :: Int -> Bool -> Bool
applyRule n state
    | n == 3 = True
    | state && n == 2 = True
    | otherwise = False

-- 4DIMENSIONAL

getStartingMap4 :: [String] -> Map.Map (Int, Int, Int, Int) Bool
getStartingMap4 input = Map.fromList $ getQuadruple [0 .. length input] [0 .. length (head input)] input

getQuadruple ::  [Int] -> [Int] -> [String] -> [((Int,Int,Int,Int), Bool)]
getQuadruple rows cols input = concat [[((0, 0, row, col), char == '#') | (col,char) <- tuples] | (row, tuples)<-zip rows (map (zip cols) input)]

quadrupleCartesian :: [Int] -> [Int] -> [Int] -> [Int] -> [(Int,Int,Int,Int)]
quadrupleCartesian ws zs ys xs = concat [concat [concat[[(w,z,y,x) | x <- xs] | y <- ys] | z <- zs] | w <- ws ]

adjustMap4 :: [(Int, Int,Int,Int)] -> Map.Map (Int,Int,Int,Int) Bool -> Map.Map (Int,Int,Int,Int) Bool
adjustMap4 [] set = set
adjustMap4 (tr:triples) chart = if isNothing value then adjustMap4 triples (Map.insert tr False chart) else adjustMap4 triples chart
    where value = chart Map.!? tr

simulate4 :: Int -> Map.Map (Int,Int,Int,Int) Bool -> [(Int,Int)] -> Map.Map (Int,Int,Int,Int) Bool
simulate4 0 space _ = space
simulate4 steps space [w,z,y,x] = simulate4 (steps-1) (calcStep4 (adjustMap4 (quadrupleCartesian [(fst w) -1 .. (snd w) + 1] [(fst z) -1 .. (snd z) + 1] [(fst y) -1 .. (snd y) +1] [(fst x) -1  .. (snd x) + 1]) space)) [newW, newZ, newY, newX]
    where newW = (fst w -1, snd w + 1)
          newZ = (fst z -1, snd z + 1)
          newY = (fst y -1, snd y + 1)
          newX = (fst x -1, snd x + 1)

calcStep4 :: Map.Map (Int,Int,Int,Int) Bool -> Map.Map (Int,Int,Int,Int) Bool
calcStep4 space = Map.mapWithKey (modify4 space) space

modify4 :: Map.Map (Int,Int,Int,Int) Bool -> (Int,Int,Int,Int) -> Bool -> Bool
modify4 space (w,z,y,x) value = applyRule (nNeighbors4 state (quadrupleCartesian (ext w) (ext z) (ext y) (ext x)) space) state
    where ext n = [n-1, n, n+1]
          state = fromMaybe False (space Map.!? (w,z,y,x))

nNeighbors4 :: Bool -> [(Int,Int,Int,Int)] -> Map.Map (Int,Int,Int,Int) Bool -> Int
nNeighbors4 _ [] space = 0
nNeighbors4 True coords space = nNeighbors4 False coords space - 1 
nNeighbors4 False (c:coords) space = if fromMaybe False (space Map.!? c) then 1 + nNeighbors4 False coords space else nNeighbors4 False coords space