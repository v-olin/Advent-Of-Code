{-# LANGUAGE TupleSections #-}

{-
    PowerShell Timing: ('cabal run')
        Milliseconds      : 123
        Ticks             : 1238755
-}

import System.IO()
import Data.List.Split

type Fishes = [Int]

emptyFish :: Fishes
emptyFish = replicate 9 0

main :: IO()
main = do vals <- getVals "input.txt"
          let fs = initFishes emptyFish vals
          print ("Part 1: " ++ show (sum $ updateCycle fs 80))
          print ("Part 1: " ++ show (sum $ updateCycle fs 256))

getVals :: FilePath -> IO [Int]
getVals path = do contents <- readFile path
                  return (map (read::String->Int) (splitOn "," contents))

addAt :: Fishes -> Int -> Int -> Fishes
addAt fs i x = take i fs ++ [(fs !! i) + x] ++ drop (i+1) fs

setAt :: Fishes -> Int -> Int -> Fishes
setAt fs i x = take i fs ++ [x] ++ drop (i+1) fs

initFishes :: Fishes -> [Int] -> Fishes
initFishes fs [] = fs
initFishes fs (x:xs) = initFishes (addAt fs x 1) xs

shiftL :: Fishes -> Fishes
shiftL fs = drop 1 fs ++ take 1 fs

updateCycle :: Fishes -> Int -> Fishes
updateCycle fs d
    | d == 0 = fs
    | otherwise = updateCycle fs' (d-1)
    where
        newborns = fs !! 0
        shifted = shiftL (setAt fs 0 0)
        fs' = addAt (addAt shifted 6 newborns) 8 newborns