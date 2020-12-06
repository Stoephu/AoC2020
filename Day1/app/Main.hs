module Main where
import Data.List.Split
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode  
  contents <- hGetContents handle  
  let splits = splitOn "\n" contents
  let lines = init splits
  let ints = map read lines
  let diff x = map (x - ) ints 
  let elemdiff = map elem (diff 2020)
  let temp = zip elemdiff (diff 2020)
  let hits = [b | (a, b) <- temp , a ints]
  let result = product hits
  putStrLn "Solution to Part 1:"
  print result
  let fstdifference = diff 2020
  let snddifference = map diff fstdifference
  let hits2 = uniqify [x | x <- flatten snddifference,  x `elem` ints]
  let result2 = product hits2
  print hits2
  putStrLn "Solution to Part 2:"
  print result2
  hClose handle

flatten [] = []
flatten (ls:ls') = ls ++ flatten ls'

uniqify [] = []
uniqify (x:xs) = x : filter (/= x) (uniqify xs) 


quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

