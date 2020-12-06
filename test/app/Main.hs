module Main where

import Lib

main :: IO ()
main = do
    let bools = [True,False,True]
    let trues = filter (==True) bools
    let n = length trues
    print n
