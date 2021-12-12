import System.IO()
import Data.List
import Data.Map (Map, (!), fromList)
import qualified Data.Map as Map
import Data.List.Split (splitOn)

type Combo = ([String], [String])

main :: IO()
main = do ls <- getVals "input.txt"
          let combos = map parseCombo ls
          print (part1 combos)
          print (part2 combos)

getVals :: FilePath -> IO [String]
getVals path = do contents <- readFile path
                  return (lines contents)

stdMap :: Map Int Int
stdMap = fromList (zip [42,17,34,39,30,37,41,25,49,45] [0..9])

parseCombo :: String -> Combo
parseCombo s = (head s', s' !! 1)
    where
        s' = map (filter (/="") . splitOn " ") (splitOn "|" s)

part1 :: [Combo] -> Int
part1 cs = length $ concatMap (filter (`elem` [2,4,3,7]) . map length . snd) cs

makeMap :: [String] -> Map Char Int
makeMap ss = fromList (zip ['a'..'g'] occs)
    where
        occs = map (\c -> length (filter (==c) (concat ss))) ['a'..'g']

digitsToSum :: [Int] -> Int
digitsToSum = read . concatMap show

getNum :: Map Char Int -> String -> Int
getNum m s = stdMap ! sum (map (m !) s)

solveCombo :: Combo -> Int
solveCombo (ds,ns) = digitsToSum (map (getNum (makeMap ds)) ns)

part2 :: [Combo] -> Int
part2 cs = sum $ map solveCombo cs