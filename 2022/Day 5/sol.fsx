open System.Text.RegularExpressions
open System.Collections.Generic

let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList

let cRx = Regex(@"[A-Z]", RegexOptions.Compiled)
let iRx = Regex(@"[0-9]+", RegexOptions.Compiled)

let extract s = seq { for x in cRx.Matches s do x.Index, x.Value } 
let shorten s =
    let xs = seq { for x in iRx.Matches s do (int x.Value) } |> Seq.toList
    xs.Head :: (List.map (fun (x: int) -> (x-1)*3 + x) xs.Tail)

let addToStack (ss: (list<int * Stack<string>>)) (s: (int * string)) =
    let ss' = List.filter (fun x -> fst x = fst s) ss
    if ss'.Length > 0 then
        let stack = snd (List.head ss')
        stack.Push (snd s)
        ss
    else
        let stack = new Stack<string>()
        stack.Push (snd s)
        (fst s, stack) :: ss

let execInstr9000 (ss: (list<int * Stack<string>>)) (s: list<int>) =
    let from  = List.filter (fun x -> fst x = s.[1]) ss |> List.head |> snd
    let to_ = List.filter (fun x -> fst x = s.[2]) ss |> List.head |> snd
    let mutable count = s.[0]
    while count > 0 do
        to_.Push (from.Pop())
        count <- count - 1
    ss

let execInstr9001 (ss: (list<int * Stack<string>>)) (s: list<int>) =
    let from  = List.filter (fun x -> fst x = s.[1]) ss |> List.head |> snd
    let to_ = List.filter (fun x -> fst x = s.[2]) ss |> List.head |> snd
    let mutable c1 = s.[0]
    let mutable c2 = s.[0]
    let mutable (crates: seq<string>) = []
    while c1 > 0 do
        crates <- Seq.append [from.Pop()] crates
        c1 <- c1 - 1
    while c2 > 0 do
        to_.Push (crates |> Seq.head)
        crates <- crates |> Seq.tail
        c2 <- c2 - 1
    ss

let stacks = Seq.takeWhile (fun x -> x <> "") 
            >>Seq.map extract >> Seq.concat >> Seq.rev >> Seq.toList
            >> List.fold addToStack []

let instrs = input |> List.skipWhile (fun x -> x <> "") |> List.tail |> List.map shorten

let part1 = List.fold execInstr9000 (stacks input) instrs
            |> List.fold (fun (s: string) x -> s + (snd x).Peek()) ""
            |> printfn "Part 1: %s"

let part2 = List.fold execInstr9001 (stacks input) instrs
            |> List.fold (fun (s: string) x -> s + (snd x).Peek()) ""
            |> printfn "Part 2: %s"