open System

type Monkey =
    val Index: int
    val mutable Inspected: int
    val mutable Items: List<int64>
    val Operation: int64 -> int64
    val Test: int64 -> int
    new(i, items, op, test) = { Index = i; Inspected = 0; Items = items; Operation = op; Test = test }

type State =
    val Monkeys: Map<int, Monkey>
    new (monkeys: Map<int, Monkey>) = { Monkeys = monkeys }

let isNum (s:string) = s |> Seq.forall Char.IsDigit

let parseMonkey (ls: string list) =
    let index = int ls.[0].[7] - int '0'
    let items = Seq.toList ls.[1] |> List.skip 18 |> Array.ofList |> System.String.Concat
                      |> fun x -> x.Split([|", "|], StringSplitOptions.RemoveEmptyEntries)
                      |> Array.toList |> List.map int64
    let ops = ls.[2].Split(' ')
    let operator = match ops.[6] with
                    | "*" -> ( * )
                    | _ -> (+)
    let mutable (operation: int64 -> int64) = fun x -> x
    if isNum ops.[7] then
        operation <- fun x -> operator x (int64 ops.[7])
    else
        operation <- fun x -> operator x x
    let t = Seq.toList ls.[3] |> List.skip 21 |> Array.ofList |> System.String.Concat
    let tIndex: int = int ls.[4].[29] - int '0'
    let fIndex: int = int ls.[5].[30] - int '0'
    let test = fun (x: int64) -> if x % (int64 t) = 0 then tIndex else fIndex
    Monkey(index, items, operation, test)

let turn (mMap: Map<int, Monkey>) (i: int) (part: int) =
    let monkey = mMap.[i]
    monkey.Inspected <- monkey.Inspected + monkey.Items.Length
    Seq.iter (fun x ->
        let mutable x' = x
        if part = 1 then
            x' <- (monkey.Operation x) / (3: int64)
        else
            x' <- (monkey.Operation x) % (9_699_690: int64)
        let toMonkey = mMap.[monkey.Test x']
        toMonkey.Items <- toMonkey.Items @ [x']
    ) monkey.Items
    monkey.Items <- []
    mMap

let input = System.IO.File.ReadAllLines "input.txt"
            |> Array.toList
            |> List.filter (fun x -> x <> "")
            |> List.chunkBySize 6

let monkeyMap = List.map parseMonkey >> List.map (fun x -> x.Index, x) >> Map.ofList

let solve (input: string list list) (part: int) (n: int) =
        let mMap = monkeyMap input
        let mCount = mMap.Count
        let mMap' = List.fold (fun st i -> turn st (i%mCount) part) mMap [0..(mCount*n)-1]
        mMap'.Keys |> Seq.map (fun k -> mMap'.[k].Inspected) |> Seq.sortDescending
                   |> Seq.take 2 |> Seq.map (int64) |> Seq.fold (*) (1: int64)

solve input 1 20 |> printfn "Part 1: %d"
solve input 2 10000 |> printfn "Part 2: %d"