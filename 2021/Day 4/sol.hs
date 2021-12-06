import System.IO()
import Data.List
import Data.List.Split

{-
    Piece:
        Int:    Number for board
        Bool:   If piece has been marked
-}
data Piece = P Int Bool

{-
    Board:
        [[Piece]]:  Board itself
        Bool:       If board has won
        Int:        Last number to make board win
-}
data Board = Brd [[Piece]] Bool Int

-- Empty board of -1's
emptyBoard :: Board
emptyBoard = (Brd (replicate 5 (replicate 5 (P (negate 1) False))) False (negate 1))

main :: IO()
main = do lines <- getVals "input.txt"
          let nums = map (\s -> (read::String->Int) s) (splitOn "," (lines !! 0))
          let boards = filter noWhtSpc (drop 2 lines)
          let pBoards = parseBoards boards
          print (part1 pBoards nums)
          print (part2 pBoards nums)

getVals :: FilePath -> IO [String]
getVals fp = do contents <- readFile fp
                return (lines contents)

-- Filter away whitespaces or newlines
noWhtSpc :: String -> Bool
noWhtSpc s = if s == "" then False else True

-- Converts a string on numbers separated by whitespace to a list of pieces
stringToRow :: String -> [Piece]
stringToRow s = map (\i -> (P i False)) ints
    where
        ints = map (read::String->Int) (filter noWhtSpc (splitOn " " s))

-- Converts a list of strings to a list of 5x5 boards
parseBoards :: [String] -> [Board]
parseBoards s = map (\set -> (Brd set False (negate 1))) rows
    where
        pieces = map stringToRow s
        rows = chunksOf 5 pieces

-- Marks pieces on boards
applyNum :: [Board] -> Int -> [Board]
applyNum bs num = map (\b -> brd' b) bs
    where
        row' s = map (\p -> applyNumToPiece p num) s
        brd' (Brd ps b _) = (Brd (map (\r -> row' r) ps) b 0)

-- Marks piece of numbers match
applyNumToPiece :: Piece -> Int -> Piece
applyNumToPiece piece@(P n b) x
    | b = piece
    | n == x = (P n True)
    | otherwise = piece

-- Check if row has won
filtRow :: [Piece] -> Bool
filtRow r = length (filter (\(P _ b) -> b) r) == 5

-- Checks if board has won
checkIfWin :: Board -> Board
checkIfWin (Brd ps b _) = (Brd ps' (rowWin || colWin) 0)
    where
        rowWin = foldl1 (\a b -> a || b) (map (\r -> filtRow r) ps)
        colWin = foldl1 (\a b -> a || b) (map (\r -> filtRow r ) (transpose ps))
        ps' = if colWin then (transpose ps) else ps

-- Get first winning board
playFirst :: [Board] -> [Int] -> Board
playFirst bs (x:xs)
    | length win > 0 = (Brd ps' b' x)
    | otherwise = playFirst bs' xs
    where
        bs' = applyNum bs x
        win = filter (\(Brd _ b _) -> b) (map checkIfWin bs')
        winBrd@(Brd ps' b' _) = win !! 0

-- Get last winning board
playAll :: [Board] -> [Int] -> Board -> Board
playAll bs (x:xs) lastWin
    | length win > 0 = playAll bs'' xs (Brd ps' b' x)
    | otherwise = playAll bs' xs lastWin
    where
        bs' = applyNum bs x
        win = filter (\(Brd _ b _) -> b) (map checkIfWin bs')
        (Brd ps' b' _) = win !! 0
        bs'' = filter (\(Brd _ b _) -> not b) (map checkIfWin bs')
playAll _ [] lastWin = lastWin

-- Calculate sum of unmarked pieces
unmarkedSum :: Board -> Int
unmarkedSum (Brd ps _ _) = sum (map rowSum ps)
    where
        rowSum r = sum $ map (\(P a b) -> if b then 0 else a) r

part1 :: [Board] -> [Int] -> Int
part1 bs xs = (unmarkedSum winBoard) * x'
    where
        winBoard@(Brd ps' b' x') = playFirst bs xs

part2 :: [Board] -> [Int] -> Int
part2 bs xs = (unmarkedSum winBoard) * x'
    where
        winBoard@(Brd ps' b' x') = playAll bs xs emptyBoard

-- Fancy print stuff
instance Show Piece where
    show = showPiece

showPiece :: Piece -> String
showPiece (P n b)
    | b = "#" ++ show n ++ "\t"
    | otherwise = show n ++ "\t"

instance Show Board where
    show = showBoard

showBoard :: Board -> String
showBoard (Brd xs _ _) = foldl (++) "" (map printRow xs) ++ "\n\n"
    where
        printRow :: [Piece] -> String
        printRow ps = foldl (++) "" (map show ps) ++ "\n"
