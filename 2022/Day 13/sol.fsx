open System
open System.Text.RegularExpressions

type Either<'a, 'b> =
    | Number of n: 'a
    | List of ns: 'b

[<CustomEquality; NoComparison>]
type Packet =
    { v : Either<int, Packet list> }
    override this.ToString() =
        match this.v with
        | Number n -> $"Number {n}"
        | List ns ->
            let inner = String.concat "; " [for n in ns -> n.ToString()]
            String.concat "" (["["] @ [inner] @ ["]"])
    
    interface IEquatable<Packet> with
        member this.Equals other =
            match this.v, other.v with
            | Number x, Number y -> x = y
            | Number x, List ys ->
                { v = List [({v = Number x})] }.Equals { Packet.v = List ys}
            | List xs, Number y ->
                { v = List xs }.Equals { v = List [({v = Number y})] }
            | List xs, List ys ->
                match xs, ys with
                | [], [] -> true
                | [], _ -> false
                | _, [] -> false
                | x::xs', y::ys' ->
                    if x.Equals y then { v = List xs' }.Equals { v = List ys' }
                    else false

    override this.Equals other =
        match other with
        | :? Packet as p -> (this :> IEquatable<_>).Equals p
        | _ -> false

    override this.GetHashCode () =
        this.v.GetHashCode()

let cutoff (s: string) : int =
    let mutable opens = 0
    let mutable closing = false
    let mutable i = 0
    while (opens <> 0 || not closing) do
        match s.[i] with
        | '[' -> opens <- opens + 1; i <- i + 1
        | ']' -> opens <- opens - 1; i <- i + 1; closing <- true
        | _   -> i <- i + 1
    i

let rec parse (s: string) : Packet =
    let irx = Regex(@"[0-9]+", RegexOptions.Compiled)
    let mutable str = s.Substring 1
    let mutable p: Packet list = []
    while str <> "" do
        match str.[0] with
        | '[' ->
            let last = cutoff str
            let inner = parse str.[0..last]
            p <- p @ [inner]
            str <- str.[last+1..]
        | ']' -> str <- ""
        | ',' -> str <- str.Substring 1
        | _   ->
            if Char.IsDigit str.[0] then
                let n = irx.Match(str).Value |> int
                str <- str.Substring <| n.ToString().Length
                p <- p @ [{ v = Number n}] // [Packet(n)]
            else failwith "unknown char"
    { v = List p } //Packet p

type CompareResult =
    | InOrder = -1
    | Continue = 0
    | OutOfOrder = 1

let rec compare (l: Packet, r: Packet) : CompareResult =
    match l.v, r.v with
    | Number x, Number y ->
        if x < y then CompareResult.InOrder
        elif x = y then CompareResult.Continue
        else CompareResult.OutOfOrder
    | Number x, List ys ->
        compare ({v = List [({ v = Number x })]}, {v=List ys})
    | List xs, Number y ->
        compare ({v = List xs}, {v = List [({ v = Number y})]})
    | List xs, List ys ->
        compareLists (xs, ys)

and compareLists (l: Packet list, r: Packet list) =
    match l, r with
    | [], [] -> CompareResult.Continue
    | [], _ -> CompareResult.InOrder
    | _, [] -> CompareResult.OutOfOrder
    | x::xs, y::ys ->
        let res = compare (x, y)
        match res with
        | CompareResult.Continue -> compareLists (xs, ys)
        | _ -> res

let rec chunk (s: 'T list) =
    let pair = s |> List.take 2 |> fun l -> (l.[0], l.[1])
    let rest = s |> List.skip 2
    [pair] @ (if rest.Length > 0 then chunk rest else [])

let input = System.IO.File.ReadAllLines "input.txt"
            |> Array.toList
            |> List.filter (fun s -> s <> "")
            |> List.map parse

let part1 = input |> chunk |> List.map compare
            |> fun l -> List.zip l [1..l.Length]
            |> fun l ->
                List.filter (fun (r, _) -> r = CompareResult.InOrder) l
            |> List.map snd |> List.sum

let packetComparer (l: Packet) (r: Packet) : int = int <| compare (l, r)

let keys = [for s in ["[[2]]"; "[[6]]"] -> parse s]
let input2 = input @ keys
            |> List.sortWith packetComparer

let part2 = keys
            |> List.map (fun k ->
                List.findIndex (fun e -> e.Equals k) input2)
            |> List.map (fun i -> i + 1)
            |> List.fold (*) 1

printfn "Part 1: %d" part1
printfn "Part 2: %d" part2