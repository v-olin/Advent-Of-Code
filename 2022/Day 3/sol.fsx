let splitEq (s: string) =
    let a = s.[0..(s.Length/2)-1] |> Seq.toList // |> List.map int
    let b = s.[(s.Length/2)..] |> Seq.toList // |> List.map int
    (a, b)

let dict =
    (List.zip ['a'..'z'] [1..26]) @ (List.zip ['A'..'Z'] [27..52])
    |> Map.ofList

let intersect (a: list<char>) (b: list<char>) =
    a |> List.filter (fun x -> List.contains x b)

let intersect2 (a: list<char>) (b: list<char>) (c: list<char>) =
    a |> List.filter (fun x -> List.contains x b && List.contains x c)

let input = System.IO.File.ReadAllLines "input.txt"
            |> Array.toList

let score = List.map (Seq.distinct >> List.ofSeq)
            >> List.map (fun x -> x |> List.map (fun y -> dict.[y]))
            >> List.map (fun x -> x |> List.sum)
            >> List.sum

let part1 = input
            |> List.map splitEq
            |> List.map (fun (a, b) ->(intersect a b))
            |> score
            |> printfn "Part 1: %d"

let part2 = input
            |> List.map (Seq.toList)
            |> (Seq.chunkBySize 3 >> Seq.toList)
            |> List.map (Array.toList >> List.ofSeq)
            |> List.map (fun x -> intersect2 (x.Item 0) (x.Item 1) (x.Item 2) )
            |> score
            |> printfn "Part 2: %d"