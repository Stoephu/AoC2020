module Main where

import System.IO
import Data.List.Split
import qualified Data.List as List

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let splittedStrings = init $ splitOn "\n"  contents
    let pairs = map (splitAt 7) splittedStrings
    let binaries = map toBinary pairs
    let robin = [b1 | (b1,_) <- binaries]
    let colbin = [b2 | (_,b2) <- binaries]
    let rowColumn = zip (map (getInt 7) robin)(map (getInt 3) colbin)
    let ids = map (\(a,b) -> a*8 + b) rowColumn
    let maxId = maximum ids
    --print binaries
    print rowColumn
    print maxId
    -- part 2
    let minId = minimum ids
    let missingFront = [0,1.. minId-1]
    let missingBack = [maxId +1 .. 1023]
    ---let result = 1024 - (sum  (map (+1) (missingFront ++ ids ++ missingBack)) `mod` 1024) - 1
    let result = 1023 - (sum  ( (missingFront ++ ids ++ missingBack)) `mod` 1023 ) 
    print (List.sort $ missingFront ++ ids ++ missingBack)
    print minId
    print maxId
    print result 
    print (result `elem` ids)
    {-let triples = zip3 ids robin colbin
    print triples
    let sorted = List.sort triples
    print sorted
    let leftOver = (foldl1 listXor robin, foldl1 listXor colbin)
    print leftOver
    print ((\(a,b) -> a*8 + b) (getInt 7 (fst leftOver), getInt 3 (snd leftOver)))-}
    hClose handle

toBinary (b1,b2) = (binify b1 , binify b2)

binify :: String -> [Bool]
binify "" = []
binify (c:ls) = (c == 'B' || c == 'R') : binify ls

getInt :: Int -> [Bool] -> Int
getInt 0 [] = 0
getInt 0 _ = 0
getInt n (b:bin) = if b then 2^(n-1) + getInt (n-1) bin else getInt (n-1) bin

listXor as bs = [a /= b | (a,b) <- zip as bs]
{-
getLeftOver :: [Bool] -> [[Bool]] -> [Bool]
getleftOver current [] = current
getLeftOver current (l:ls) = getLeftOver [b1 /= b2 | (b1,b2) <- zip current l] ls
getLeftOver [] _ = error "Current is empty"

generateSeats start stop 
    | start == stop = []
    | otherwise = start1 : generateSeats start1 stop
        where start1 = add1 start
              add1 = foldr (\x acc -> (x /= head acc) : acc) [True] 
              -}