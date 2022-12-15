open System
open System.Text.RegularExpressions

type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b

let isLeft = function
  | Left _ -> true
  | _      -> false

let isRight = function
  | Right _ -> true
  | _      -> false

// type Packet = 
type Packet =
    val mutable Integers: int list
    val mutable Nested: Packet list
    new (i, p) = { Integers = i; Nested = p }

type Pair = 
    val Left: Packet
    val Right: Packet
    new (l, r) = { Left = l; Right = r }

let obRx = Regex(@"\[", RegexOptions.Compiled)
let cbRx = Regex(@"\]", RegexOptions.Compiled)

let parseList (s: string) =
    let rx = Regex(@"[0-9]+", RegexOptions.Compiled)
    let is = seq { for x in rx.Matches s do (int x.Value) } |> Seq.toList
    Packet(is, [])

let rec parsePacket (s: string) =
    printfn "Parsing: %s" s
    let c = Choice
    let opens = obRx.Matches s
    let closed = cbRx.Matches s
    if opens.Count = 0 then
        Left ()
    elif opens.Count = 1 then
        Right (parseList s)
    else
        let opening = opens.[1].Index
        let closing = closed.[closed.Count-2].Index
        let nested = s.[opening..closing].Split(',') |> Array.toList |> List.map (parsePacket)
        // printfn "Matches: %A" [for o in opens -> o.Index]
        // printfn "Matches: %A" [for c in closed -> c.Index]
        // let opening = opens.[1].Index
        // let closing = closed.[closed.Count-2].Index
        // printfn "O/C: %d %d" opening closing
        // printfn "Nested: %s" s.[opening..closing]
        // let nested = s.[opening..closing].Split(',')
        //                 |> Array.toList
        //                 |> List.map (parsePacket)
        // let is = s.[1..opening-1] + s.[closing+1..s.Length-2]
        // let is' = is.Split(',', StringSplitOptions.RemoveEmptyEntries)
        // let ints = is' |> Array.map (int) |> Array.toList
        Packet(ints, nested)

let parsePair (s: string list) =
    let left = parsePacket s.[0]
    let right = parsePacket s.[1]
    Pair(left, right)

let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList

parsePacket "[[1],[2,3,4]]"