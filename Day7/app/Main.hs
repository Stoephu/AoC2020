module Main where

import System.IO
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text(pack, unpack, replace)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let lines = splitOn "\n"  contents
    let personalBag = "shiny gold"
    let rulesString = map (rplSubStr " bags contain " " " . rplSubStr " bag, " " " . rplSubStr " bags, " " " . rplSubStr " bags." "" . rplSubStr " bag." "") lines
    let rulesList = map (splitOn " ") rulesString
    let rulesTuples = makeTupleList rulesList
    print rulesTuples
    let overBags = getAllOverBags [personalBag] rulesTuples
    --print overBags
    print (Set.size overBags)
    -- part 2
    let dict = Map.fromList rulesTuples
    print dict
    let nBags = countNeededBags personalBag dict - 1
    print nBags
    hClose handle

rplSubStr match replacement = unpack . replace (pack match) (pack replacement) . pack 

makeTupleList = map getTuple 

getTuple (a:b:rest) = (a ++ " " ++ b, getRest rest)

getRest :: [String] -> [(Int,String)]
getRest ["no","other"] = []
getRest [] = []
getRest (num:a:b:rest) = (read num, a ++ " " ++ b) : getRest rest

getAllOverBags :: [String] -> [(String, [(Int,String)])] -> Set.Set String
getAllOverBags [] _ = Set.empty :: Set.Set String
getAllOverBags (bagName:rest) rules = Set.union (Set.fromList hits) (getAllOverBags (rest ++ hits) rules)
    where hits = [overBag | (overBag, tuples) <- rules, bagName `elem` [bag | (_,bag) <- tuples ]]

countNeededBags :: String -> Map.Map String [(Int,String)] -> Int
countNeededBags bagName dict = sum [ fst tuple * countNeededBags (snd tuple) dict | tuple <- dict Map.! bagName] + 1