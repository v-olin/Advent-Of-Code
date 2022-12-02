let toPlay (s : string) =
    match s with
    | "A"       -> 1
    | "B"       -> 2
    | "C"       -> 3
    | "X"       -> 1
    | "Y"       -> 2
    | _         -> 3

let toTuple (xs : list<string>) = (toPlay (xs.Item 0), toPlay (xs.Item 1))
let parseRound (s : string) = s.Split ' ' |> Array.toList |> toTuple
let part1score ((a : int, b : int)) = b + ((b-a)*3+12) % 9
let part2score ((a : int, b : int)) = ((b+a)%3 + 1) + (b*3 - 3)

let rounds = System.IO.File.ReadAllLines "input.txt"
            |> Array.toList
            |> List.map parseRound

let part1 = rounds |> List.map part1score |> List.sum |> printfn "Part 1: %d"
let part2 = rounds |> List.map part2score |> List.sum |> printfn "Part 2: %d"