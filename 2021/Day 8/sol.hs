import System.IO()
import Data.List
import Data.Map (Map, (!), fromList)
import qualified Data.Map as Map
import Data.List.Split (splitOn)

{-
    PowerShell Timing:
        Milliseconds      : 66
        Ticks             : 663851
-}

-- fst is everything left of '|', snd is everything right of '|'
type Combo = ([String], [String])

main :: IO()
main = do ls <- getVals "input.txt"
          let combos = map parseCombo ls
          print (part1 combos)
          print (part2 combos)

-- Read input
getVals :: FilePath -> IO [String]
getVals path = do contents <- readFile path
                  return (lines contents)

-- Each segment occurs n times throughout numbers 0..9,
-- if you sum these segments n's for each number you get an
-- invariant sum, map it to a value which correspond the correct digit
stdMap :: Map Int Int
stdMap = fromList (zip [42,17,34,39,30,37,41,25,49,45] [0..9])

-- Parsing
parseCombo :: String -> Combo
parseCombo s = (head s', s' !! 1)
    where
        s' = map (filter (/="") . splitOn " ") (splitOn "|" s)

-- Filter all elements which does not have 2,3,4 or 7 segments
-- in other words those numbers who aren't 1,4,7 or 8
part1 :: [Combo] -> Int
part1 cs = length $ concatMap (filter (`elem` [2,4,3,7]) . map length . snd) cs

-- Map a list of strings to a Char,Int map with occurrences of each char a..g
makeMap :: [String] -> Map Char Int
makeMap ss = fromList (zip ['a'..'g'] occs)
    where
        occs = map (\c -> length (filter (==c) (concat ss))) ['a'..'g']

-- Int list to int, base 10
digitsToSum :: [Int] -> Int
digitsToSum = read . concatMap show

-- Get decrypted num from standard sum Map
getNum :: Map Char Int -> String -> Int
getNum m s = stdMap ! sum (map (m !) s)

-- Make a map from fst, get each digit, convert to Int
solveCombo :: Combo -> Int
solveCombo (ds,ns) = digitsToSum (map (getNum (makeMap ds)) ns)

-- Sum the value of all encrypted values
part2 :: [Combo] -> Int
part2 cs = sum $ map solveCombo cs