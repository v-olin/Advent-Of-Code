open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Particle = Rock | Sand | Air

let map = new Dictionary<(int * int), Particle>()

let rec parse (s: string) =
    let rx = Regex(@"[0-9]+,[0-9]+", RegexOptions.Compiled)
    rx.Matches s |> Seq.cast|> Seq.map (fun (m: Match) -> m.Value)
        |> Seq.map toTuple |> extend
        |> Seq.iter (fun (p: (int * int)) -> 
            if not (map.ContainsKey p) then map.Add(p, Rock))

and toTuple s =
    let args = s.Split(',')
    (int args.[0], int args.[1])

and extend s =
    let xs = Seq.take ((Seq.length s) - 1) s
    let ys = Seq.skip 1 s
    Seq.zip xs ys |> Seq.map extendHelper |> Seq.concat

and extendHelper (a, b) =
    if fst a = fst b then
        let s, e = 
            if snd a <= snd b then snd a, snd b
            else snd b, snd a
        [for y in [s..e] -> ((fst a), y)]
    else
        let s, e =
            if fst a <= fst b then fst a, fst b
            else fst b, fst a
        [for x in [s..e] -> (x, (snd a))]
    
let rec simulate (x, y) =
    if freeSpot (x,y+1) then
        let next = nextLowest (x,y)
        match next with
        | Some p -> simulate p
        | None -> 
            Seq.filter (fun t -> map.Item(t) = Sand) map.Keys |> Seq.length
    else
        let next = List.tryFind freeSpot [(x-1,y+1);(x+1,y+1)]
        match next with
        | Some p -> simulate p
        | None ->
            map.Add((x,y), Sand)
            if (x,y) = (500,0) then
                Seq.filter (fun t -> map.Item(t) = Sand) map.Keys
                |> Seq.length
            else
                simulate (500,0)

and freeSpot (x, y) = not (map.ContainsKey (x,y)) || (map.Item (x,y)) = Air

and nextLowest (x,y) =
    let lowest = Seq.filter (fun (a,b) -> x = a && y < b) map.Keys
                |> Seq.sort |> Seq.toList
    match lowest with 
    | [] -> None
    | (x',y')::_ -> Some (x',y'-1)

let input = System.IO.File.ReadAllLines "input.txt"
            |> Array.toList

List.iter parse input

printfn "Part 1: %d" <| simulate (500, 0)

let floorLvl = map.Keys |> Seq.map snd |> Seq.max |> fun x -> x + 2

[for x in [(500-floorLvl-5)..(500+floorLvl+5)] -> (x, floorLvl)]
    |> List.iter (fun t -> map.Add(t, Rock))

printfn "Part 2: %d" <| simulate (500,0)