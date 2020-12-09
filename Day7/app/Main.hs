module Main where

import System.IO
import Data.List.Split
import qualified Data.Set as Set

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let paragraphs = splitOn "\n\n"  contents
    let splittedparas = map ( splitOn "\n") paragraphs

    hClose handle