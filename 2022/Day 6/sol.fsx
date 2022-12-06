let input = (System.IO.File.ReadAllLines "input.txt").[0]
    
let unbox (x: list<int * char>) = 
    let x' = List.map snd x
    x'.Length <> (List.distinct x').Length

let marker (s: string) (offset: int) =
    let s' = s.ToCharArray() |> Array.toList
    let xs = [0..(s.Length - offset)]
    List.map (fun x -> List.zip [x..x+offset-1] s'.[x..x+offset-1]) xs
    |> List.skipWhile (fun x -> unbox x)
    |> List.head |> List.head |> fst
    |> (fun x -> x + offset)

let part1 = marker input 4 |> printfn "Part 1: %d"
let part2 = marker input 14 |> printfn "Part 2: %d"