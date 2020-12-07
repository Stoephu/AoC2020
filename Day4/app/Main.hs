module Main where

import Lib
import System.IO
import Data.List.Split
import qualified Data.List
import qualified Data.Map as Map
import qualified Data.Char as DChar

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle  
    let passportStrings = splitOn "\n\n"  contents
    let passportLists = map (splitOn "\n")  [map (repl ' ' '\n') string | string <- passportStrings]
    let passportPairs = map (map (splitOn ":")) passportLists
    let passports = [Map.fromList[(k,value) | [k,value] <- passportPair] | passportPair <- passportPairs]
    let validPassports = filter hasEnoughFields passports
    let nValidPassport = length  validPassports
    --print contents
    --print passportStrings
    --print passports
    print nValidPassport

    -- part 2
    let validPassports2 = filter validFieldValues validPassports
    let nValidPassport2 = length validPassports2
    print validPassports2
    print nValidPassport2
    hClose handle

--getPassports :: [[[String]]] -> [Map]

repl :: Char -> Char -> Char -> Char
repl a b c = if a == c then b else c

hasEnoughFields :: Map.Map String String -> Bool
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
validFieldValues :: Map.Map String String -> Bool
validFieldValues pass =  byr ( pass  Map.! "byr") && iyr ( pass  Map.! "iyr") && eyr ( pass  Map.! "eyr") && hgt ( pass  Map.! "hgt") && hcl ( pass  Map.! "hcl") && ecl ( pass  Map.! "ecl" ) && pid ( pass  Map.! "pid")

byr yearstring = year >= 1920 && year <= 2002
    where year = read yearstring
iyr yearstring = year >= 2010 && year <= 2020
    where year = read yearstring
eyr yearstring = year >= 2020 && year <= 2030
    where year = read yearstring
hgt :: String -> Bool
hgt heightstring = if "cm" `Data.List.isSuffixOf`  heightstring then metricheight $ read (replace heightstring) else impheight $ read (replace heightstring)
    where metricheight height = height >= 150 && height <=193
          impheight height = height >= 59 && height <= 76
hcl haircolorstring = (head haircolorstring == '#') && isSixDigits
    where isSixDigits = foldl (\acc x -> DChar.isHexDigit x && acc) True (tail haircolorstring)


ecl eyecolorstring 
    | eyecolorstring == "amb" = True
    | eyecolorstring == "blu" = True
    | eyecolorstring == "brn" = True
    | eyecolorstring == "gry" = True
    | eyecolorstring == "hzl" = True
    | eyecolorstring == "oth" = True
    | eyecolorstring == "grn" = True
    | otherwise = False

pid idstring = length idstring == 9

replace :: String -> String
replace ('c':'m':xs) = ' ' : ' ' : replace xs
replace ('i':'n':xs) = ' ' : ' ' : replace xs
replace (x:xs)       = x : replace xs
replace ""           = ""
