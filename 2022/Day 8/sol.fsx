let inline charToInt c = (int c - int '0') // |> float

let around (m: List<List<int>>) (x: int) (y: int) =
    if 
    let ns = [(x-1, y-1); (x+1, y-1); (x-1, y+1); (x+1, y+1)]
    let me = m.[y].[x]
    printfn $"{me} at {x}, {y}"
    let eqs = List.map (fun n -> m.[snd n].[fst n] < me) ns
                |> List.fold (||) false
    printfn $"Visible: {eqs}"
    eqs

let zipper (a: List<int>) (b: List<int>) =
    let zipper'  (b: List<int>) (a: int) = List.map (fun x -> (a, x)) b
    List.map (zipper' b) a |> List.concat

let part1 input = 
    let parsed = List.map (Seq.toList >> List.map charToInt) input
    let xs = [1..(parsed.[0].Length - 2)]
    let ys = [1..(parsed.Length - 2)]
    let is = zipper xs ys
    let res = List.map (fun i -> around parsed (fst i) (snd i)) is
    let inner = List.fold (fun s' (x: bool) -> if x then s' + 1 else s') 0 res
    let outer = (parsed.Length-1) * 2 + (parsed.[0].Length-1) * 2
    printfn "Part 1: %d" (inner)


let input = System.IO.File.ReadAllLines "test.txt" |> Array.toList
part1 input

// let parsed = List.map (Seq.toList >> List.map charToInt) input
// let xs = [1..(parsed.[0].Length - 2)]
// let ys = [1..(parsed.Length - 2)]
// let is = zipper xs ys//
// printfn "%A" is