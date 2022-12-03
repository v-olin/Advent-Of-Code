let splitEq (s: string) =
    let a = s.[0..(s.Length/2)-1] |> Set.ofSeq // |> List.map int
    let b = s.[(s.Length/2)..] |> Set.ofSeq // |> List.map int
    (a, b)

let dict =
    (List.zip ['a'..'z'] [1..26]) @ (List.zip ['A'..'Z'] [27..52])
    |> Map.ofList

let input = System.IO.File.ReadAllLines "input.txt"
            |> Array.toList

let score = Seq.map (Seq.distinct >> Set.ofSeq)
            >> Seq.map (fun x -> x |> Set.map (fun y -> dict.[y]))
            >> Seq.map (fun x -> x |> Set.toList |> List.sum)
            >> Seq.sum

input   |> List.map (splitEq)
        |> Seq.ofList
        |> Seq.map (fun (a, b) -> Set.intersect a b)
        |> score
        |> printfn "Part 1: %d"

input   |> List.map (Set.ofSeq)
        |> Seq.chunkBySize 3
        |> Seq.map Set.intersectMany
        |> score
        |> printfn "Part 2: %d"