import System.IO()
import Data.List.Split

main :: IO()
main = do nums <- getVals "input.txt"
          print (part1 nums)
          print (part2 nums)

getVals :: FilePath -> IO [String]
getVals fp = do contents <- readFile fp
                return (lines contents)

toDigit :: String -> Int -> Int
toDigit s n = (read::String->Int) [(s !! n)]

mostCommons :: [String] -> Int -> [Int]
mostCommons s n
    | n >= length (s !! 0) = []
    | dig s < thold = [0] ++ mostCommons s (n + 1)
    | dig s >= thold = [1] ++ mostCommons s (n + 1)
    where
        dig s' = sum $ map (\str -> (read::String->Int) [(str !! n)]) s'
        thold = (length s) `div` 2

binToDec :: [Int] -> Int
binToDec xs = foldr (\x y -> x + 2*y) 0 (reverse xs)

inverse :: Int -> Int -> Int
inverse x n = (binToDec (replicate n 1)) - x

filterOnBit :: [String] -> Int -> Int -> String
filterOnBit s n t
    | length s == 1 = s !! 0
    | t == 0 = filterOnBit sCo2 (n + 1) t
    | otherwise = filterOnBit sOxy (n + 1) t
    where
        a = filter (\c -> (c !! n) == '0') s
        b = filter (\c -> (c !! n) == '1') s
        sOxy = if (length a <= length b) then b else a -- More 1's
        sCo2 = if (length a <= length b) then a else b  -- More 0's

rating :: [String] -> Int -> Int
rating s t = binToDec $ map (\c -> (read::String->Int) c) (chunksOf 1 (filterOnBit s 0 t))

part1 :: [String] -> Int
part1 xs = gamma * epsilon
        where
            gamma = binToDec (mostCommons xs 0)
            epsilon = inverse gamma (length (xs !! 0))

part2 :: [String] -> Int
part2 xs = (rating xs 1) * (rating xs 0)