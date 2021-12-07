import System.IO()
import Data.List.Split

{-
    PowerShell Timing:
        Milliseconds      : 302
        Ticks             : 3026044
-}

main :: IO()
main = do vals <- getVals "input.txt"
          let as = [(minimum vals)..(maximum vals)]
          print ("Part 1: " ++ show (solve vals as 1))
          print ("Part 2: " ++ show (solve vals as 2))

getVals :: FilePath -> IO [Int]
getVals path = do contents <- readFile path
                  return (map (read::String->Int) (splitOn "," contents))

getFuel :: [Int] -> Int -> Int -> Int
getFuel (x:xs) p m = x' + getFuel xs p m
    where
        x' = if p == 1 then abs (x-m) else sum [1..(abs (x-m))]
getFuel [] _ _ = 0

solve :: [Int] -> [Int] -> Int -> Int
solve xs ls p = minimum $ map (getFuel xs p) ls