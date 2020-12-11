module Main where

import Lib

import System.IO
import Data.List.Split
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
    let instructions = map (tuplify2 . (splitOn " "))  lines
    let dicInstruc = Map.fromList $ zip [0 .. length instructions] instructions
    let accu = runCodeTillLoop dicInstruc 0 Set.empty
    print accu
    -- Part 2
    let accu2 = tryTerminate dicInstruc 0 False Set.empty
    print accu2
    hClose handle


tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

runCodeTillLoop :: Map.Map Int (String, String) -> Int -> Set.Set Int -> Int
runCodeTillLoop dict adr visited
    | Set.member adr visited = 0
    | instruc == "nop" = runCodeTillLoop dict (adr+1) (Set.insert adr visited)
    | instruc == "acc" = number + runCodeTillLoop dict (adr+1) (Set.insert adr visited)
    | instruc == "jmp" = runCodeTillLoop dict (adr+number) (Set.insert adr visited)
        where tuple = dict Map.! adr
              instruc = fst tuple
              number = if sign == '+' then read num else negate (read num) :: Int
              (sign:num) = snd tuple

tryTerminate :: Map.Map Int (String, String) -> Int -> Bool -> Set.Set Int -> Maybe Int
tryTerminate dict adr changed visited
    | Set.member adr visited = trace ("Terminator infinite Loop at " ++ show adr) Nothing
    | adr == Map.size dict = trace ("Terminator end of Instructions at " ++ show adr) Just 0
    | instruc == "nop" && changed =   trace (show adr) tryTerminate dict (adr+1) changed (Set.insert adr visited)
    | instruc == "acc" = trace ("step at " ++ show adr) (if isNothing checkacc then Nothing else Just (number + fromMaybe 0 checkacc))
    | instruc == "jmp" && changed = trace ("step at " ++ show adr)                 tryTerminate dict (adr+number) changed (Set.insert adr visited)
    | instruc == "nop" && not changed = trace ("testing alternate nop -> jump at " ++ show adr)  (if isNothing checkjmp then tryTerminate dict (adr+1) False (Set.insert adr visited) else checkjmp)
    | instruc == "jmp" && not changed = trace ("testing alternate jump -> nop at " ++ show adr)  (if isNothing checknop then tryTerminate dict (adr+number) False (Set.insert adr visited) else checknop)
        where tuple = dict Map.! adr
              instruc = fst tuple
              number = trace ("number " ++ show tuple ++" at " ++ show adr) (if sign == '+' then read num else negate (read num) :: Int)
              (sign:num) = snd tuple
              checkjmp = tryTerminate (Map.insert adr ("jmp", if number >= 0 then "+" ++ show number else show number) dict) adr True visited
              checknop = tryTerminate (Map.insert adr ("nop", if number >= 0 then "+" ++ show number else show number) dict) adr True visited
              checkacc = tryTerminate dict (adr+1) changed (Set.insert adr visited)
