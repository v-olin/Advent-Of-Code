import System.IO()
import Data.List
import Data.List.Split

main :: IO()
main = do ls <- getVals "test.txt"
          let parsed = parseCombos ls
          print ("Part 1: " ++ show (part1 parsed))
        --   print occurrences
        --   print stdSums
        --   print (sumFromStr "abcde")
        
          print ("Part 2: " ++ show (part2 parsed))

type Combo = ([String], [String])
type Signal = [(Int, String)]

p1digits :: [Int]
p1digits = [2, 4, 3, 7]

occurrences :: [(Char, Int)]
occurrences = zip ['a'..'g'] [8,6,8,7,4,9,7]

stdSums :: [(Int, Int)]
stdSums = zip [0..9] [42,17,34,39,30,37,41,25,49,45]

segments :: [String]
segments = splitOn " " s
    where
        s = "abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg"

findOccs :: String -> Char -> [(Char, Int)]
findOccs s c = zip ['a'..'g'] ns
    where
        ns = map (length . (filter (\c' -> c' == c) s)) ['a'..'g']

-- Filter away whitespaces or newlines
noWhtSpc :: String -> Bool
noWhtSpc s = if s == "" then False else True

getVals :: FilePath -> IO [String]
getVals path = do contents <- readFile path
                  return (lines contents)

parseCombo :: String -> Combo
parseCombo s = (s' !! 0, s' !! 1)
    where
        s' = map (filter noWhtSpc) (map (splitOn " ") (splitOn "|" s))

parseCombos :: [String] -> [Combo]
parseCombos ss = map parseCombo ss

checkLen :: [String] -> Int
checkLen ss = length $ intersect ss' p1digits
    where
        ss' = map (\s -> length s) ss

part1 :: [Combo] -> Int
part1 cs = foldl (+) 0 $ map (\(a,b) -> checkLen b) cs

sumFromStr :: [(Char, Int)] -> String -> Int
sumFromStr occs s = sum $ map (\c -> (snd ((fromChar c) !! 0))) s
    where
        fromChar c = filter (\oc -> if (fst oc) == c then True else False) occs

digitFromSum :: [(Int, Int)] -> Int -> Int
digitFromSum sums n = snd (std !! 0)
    where
        std = filter (\p -> if (snd p) == n then True else False) sums

digitsToSum :: [Int] -> Int
digitsToSum = read . concatMap show

part2 :: [Combo] -> Int
part2 cs = sum $ map cs' cs
    where
        cs' xs = digitsToSum $ map digitFromSum (map (sumFromStr) (snd xs))
        