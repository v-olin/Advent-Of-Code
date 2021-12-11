{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.Matrix
import Data.List.Split (chunksOf, splitOn)

main :: IO()
main = do vals <- getVals "input.txt"
          let mtrx = fromLists (map filterWhtSpc vals)
          print (part1 mtrx)
          print (part2 mtrx)

getVals :: FilePath -> IO [[String]]
getVals path = do contents <- readFile path
                  return (map (splitOn "") (lines contents))

filterWhtSpc :: [String] -> [Int]
filterWhtSpc = map (read::String->Int) . filter (/= "")

offsets :: Matrix Int -> (Int, Int) -> [(Int, Int)]
offsets m (x,y) = filter (\(a,b) -> not (a < 1 || a > h || b < 1 || b > l)) ps
    where
        ps = map (\(a,b) -> (a+x,b+y)) [(0,-1),(1,0),(0,1),(-1,0)]
        (l,h) = (ncols m, nrows m)

basin :: Matrix Int -> (Int, Int) -> Bool
basin m (x,y) = getElem x y m /= 9

checkLow :: Matrix Int -> (Int, Int) -> Bool
checkLow m p = all (\(x,y) -> n < getElem x y m) (offsets m p)
    where
        n = uncurry getElem p m

findLows :: Matrix Int -> [(Int, Int)]
findLows m = filter (checkLow m) ps
    where
        (l, h) = (ncols m, nrows m)
        ps = concatMap (\x' -> map (x',) [1..l]) [1..h]

part1 :: Matrix Int -> Int
part1 m = sum $ map ((+ 1) . (\(x,y) -> getElem x y m)) (findLows m)

fill :: Matrix Int -> [(Int, Int)] -> [(Int, Int)] -> Int
fill _ [] vstd = length (nub vstd)
fill m (p:ps) vstd = fill m ps' (vstd ++ [p])
    where
        ps' = filter (basin m) (ps ++ (offsets m p \\ vstd))

part2 :: Matrix Int -> Int
part2 m = product (take 3 (reverse (sort (map (\p -> fill m [p] []) lows))))
    where
        lows = findLows m
