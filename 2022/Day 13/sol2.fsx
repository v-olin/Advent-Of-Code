open System

let isStrNum (s:string) = s |> Seq.forall Char.IsDigit
let isCharNum (c: char) = c <= '9' && c >= '0'
let extractAt (s: string) (i: int) =
    let s' = s |> Seq.toList |> List.skip i
            |> List.takeWhile (fun c -> c <= '9' && c >= '0')
            |> List.map string |> String.concat ""
    (int s', i + s'.Length)

let compare' (s1: string) (s2: string) (i: int) (j: int) =
    let mutable lvl = 0
    let mutable (i, j) = (0,0)
    let (left, right) = if (s1.Length < s2.Length) then (s1,s2) else (s2,s1)
    let (l, r) = (left |> Seq.toList, right |> Seq.toList)
    let mutable inOrder = true
    while i < l.Length && j < r.Length && inOrder do
        let lc = l.[i]
        let rc = r.[j]
        if lc = '[' && rc = ']' || rc = '[' && lc = ']' then
            inOrder <- false
        elif lc = ']' && rc = '[' then
            inOrder <- false
        if lc = '[' || rc = '[' then
            lvl <- lvl + 1
            if lc = '[' then
                i <- i + 1
            if rc = '[' then
                j <- j + 1
        elif lc = ']' || rc = ']' then
            lvl <- lvl - 1
            if lc = ']' then
                i <- i + 1
            if rc = ']' then
                j <- j + 1
        elif isCharNum lc && isCharNum rc then
            let (lc', i') = extractAt left i
            let (rc', j') = extractAt right j
            if lc' > rc' then
                inOrder <- false
            i <- i'
            j <- j'
        elif isCharNum lc && not (isCharNum rc) then
            j <- j + 1
        elif isCharNum rc && not (isCharNum lc) then
            i <- i + 1
        else
            i <- i + 1
            j <- j + 1
    if j = r.Length && i < l.Length then
        inOrder <- false
    inOrder

let compare (ss: string list) =
    compare' ss.[0] ss.[1] 0 0

let input = System.IO.File.ReadAllLines "test.txt"
            |> Array.toList
            |> List.filter (fun x -> x <> "")
            |> List.chunkBySize 2

let part1 = input |> List.map compare
                         |> fun x -> x |> List.zip [1..x.Length]
                         |> printfn "%A"
                        //  |> List.filter (fun x -> x) |> List.length |> printfn "Part 1: %d"