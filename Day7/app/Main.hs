module Main where

import System.IO
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text(pack, unpack, replace)

main :: IO ()
main = do
    handle <- openFile "test.txt" ReadMode
    contents <- hGetContents handle
    let lines = splitOn "\n"  contents
    let personalBag = "shiny gold"
    let rulesString = map (rplSubStr " bags contain " " " . rplSubStr " bag, " " " . rplSubStr " bags, " " " . rplSubStr " bags." "" . rplSubStr " bag." "") lines
    let rulesList = map (splitOn " ") rulesString
    let rulesTuples = makeTupleList rulesList
    print rulesTuples
    hClose handle

rplSubStr match replacement = unpack . replace (pack match) (pack replacement) . pack 

makeTupleList = map getTuple 

getTuple (a:b:rest) = (a ++ " " ++ b, getRest rest)
getRest :: [String] -> [(Int,String)]
getRest ["no","other"] = []
getRest [] = []
getRest (num:a:b:rest) = (read num, a ++ " " ++ b) : getRest rest