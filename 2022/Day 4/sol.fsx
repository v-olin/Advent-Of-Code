type Pair = Set<int> * Set<int>

let toPair (s: list<string[]>) =
    let a = s.[0][0] |> int
    let b = s.[0][1] |> int
    let c = s.[1][0] |> int
    let d = s.[1][1] |> int
    let x = Set.ofList [a..b] //|> Seq.distinct |> Set.ofSeq
    let y = Set.ofList [c..d] //|> Seq.distinct |> Set.ofSeq
    (x, y)

let input = System.IO.File.ReadAllLines "input.txt"
            |> Array.toList
            |> List.map (fun x -> x.Split(',') |> Array.toList)
            |> List.map (List.map (fun x -> x.Split('-')))
            |> List.map toPair

let fullOverlap p =
    let (x, y) = p
    let i = Set.intersect x y
    i.Count = x.Count || i.Count = y.Count

let overlap p =
    let (x, y) = p
    let i = Set.intersect x y
    i.Count > 0

input   |> List.map fullOverlap
        |> List.filter (fun x -> x = true)
        |> List.length
        |> printfn "Part 1: %d"

input   |> List.map overlap
        |> List.filter (fun x -> x = true)
        |> List.length
        |> printfn "Part 2: %d"