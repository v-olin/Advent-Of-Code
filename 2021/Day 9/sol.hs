{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.Matrix
import Data.List.Split (chunksOf, splitOn)

main :: IO()
main = do vals <- getVals "test.txt"
          let mtrx = fromLists (map filterWhtSpc vals)
          print (part1 mtrx)

getVals :: FilePath -> IO [[String]]
getVals path = do contents <- readFile path
                  return (map (splitOn "") (lines contents))

filterWhtSpc :: [String] -> [Int]
filterWhtSpc = map (read::String->Int) . filter (/= "")

internalsM :: Matrix Int -> [Matrix Int]
internalsM m = map (\(x,y) -> submatrix x (x+2) y (y+2) m) ps
    where
        (x,y) = (nrows m, ncols m)
        ps = concatMap (\x' -> map (x',) [1..(y-2)]) [1..(x-2)]

bordersM :: Matrix Int -> [Matrix Int]
bordersM m = map (\(x',y') -> submatrix x' (x'+1) y' (y'+1) m) ps2
    where
        (x,y) = (nrows m, ncols m)
        ps1 = concatMap (\x' -> map (x',) [1..(y-1)]) [1,x-1]
        ps2 = map (,1) [1..(x-1)]
        ps = nub (ps1 ++ ps2)

bordersRM :: Matrix Int -> [Matrix Int]
bordersRM m = map ((\(x',y') -> submatrix x' (x'+1) y' (y'+1) m) . (,y-1)) [1..(x-1)]
    where
        (x,y) = (nrows m, ncols m)

submtrx :: Matrix Int -> [Matrix Int]
submtrx m = internalsM m ++ bordersM m

lowpoint :: Matrix Int -> Bool
lowpoint m
    | ncols m == 2 = all (getElem 1 1 m <) (drop 1 (toList m))
    | otherwise = all (getElem 2 2 m <) (take 4 (toList m) ++ drop 5 (toList m))

lowpoint' :: Matrix Int -> Bool
lowpoint' m = all (getElem 1 2 m <) (take 1 (toList m) ++ drop 2 (toList m))

getLow :: Matrix Int -> Int
getLow m
    | ncols m == 2 = getElem 1 1 m
    | otherwise = getElem 2 2 m

getLow' :: Matrix Int -> Int
getLow' = getElem 1 2

part1 :: Matrix Int -> [Matrix Int]
part1 m = ltb ++ rht
    where
        ltb = filter lowpoint (bordersM m)
        rht = filter lowpoint' (bordersRM m)