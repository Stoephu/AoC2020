module Main where

import Lib
import System.IO
import Data.List.Split
import qualified Data.Map as Map

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle  
    let passportStrings = init $ splitOn "\n\n" contents
    let passportLists = map (splitOn "\n")  [map (repl ' ' '\n') string | string <- passportStrings]
    let passportPairs = map (map (splitOn ":")) passportLists
    let passports = [Map.fromList[(k,value) | [k,value] <- passportPair] | passportPair <- passportPairs]
    let nValidPassport = length $ filter (==True) (map hasEnoughFields passports)
    print nValidPassport
    hClose handle

--getPassports :: [[[String]]] -> [Map]

repl :: Char -> Char -> Char -> Char
repl a b c = if a == c then b else c

hasEnoughFields :: (Map.Map String String) -> Bool
hasEnoughFields pass = length ( filter fields (Map.keys pass) ) >= 7

fields field
    | field == "byr" = True
    | field == "iyr" = True
    | field == "eyr" = True
    | field == "hgt" = True
    | field == "hcl" = True
    | field == "ecl" = True
    | field == "pid" = True
    | otherwise = False