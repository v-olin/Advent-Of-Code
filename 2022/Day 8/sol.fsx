let zipper (a: List<'T>) (b: List<'T>) =
    let zipper'  (b: List<'T>) (a: 'T) = List.map (fun x -> (a, x)) b
    List.map (zipper' b) a |> List.concat

let checkVec (m: List<List<int>>) (x: int) (y: int) =
    let vec = m.[y]
    let me = m.[y].[x]
    let a = List.take x vec |> List.map (fun x -> x < me) |> List.fold (&&) true
    let b = List.skip (x+1) vec |> List.map (fun x -> x < me) |> List.fold (&&) true
    a || b

let checkPos (m: List<List<int>>) =
    let xys = zipper [1..(m.[0].Length)-2] [1..m.Length-2]
    let hs = List.map (fun t -> checkVec m (fst t) (snd t)) xys
    let vs = List.map (fun t -> checkVec (List.transpose m) (snd t) (fst t)) xys
    let zs = List.zip hs vs
    let is = List.map (fun t -> if (fst t) || (snd t) then 1 else 0) zs |> List.sum
    is + (m.Length)*2 + (m.[0].Length)*2 - 4

let rec foldWhile' (s: int) (h: int) (l: List<int>)  =
    if l.Length = 0 then s
    elif l.Head < h then (foldWhile' (s+1) h l.Tail)
    else s+1

let views (m: List<List<int>>) (x: int) (y: int) (me: int) =
    let a = List.take x m.[y] |> List.rev |> foldWhile' 0 me
    let b = List.skip (x+1) m.[y] |> foldWhile' 0 me
    (a,b)

let viewDistance (m: List<List<int>>) (x: int) (y: int) =
    let m' = List.transpose m
    let me = m.[y].[x]
    let (l, r)  = views m x y me
    let (u, d) = views (List.transpose m) y x me
    l * r * u * d

let findMaxView (m: List<List<int>>) =
    let xs = [1..(m.[0].Length)-2]
    let ys = [1..m.Length-2]
    let xys = zipper xs ys
    List.map (fun t -> viewDistance m (fst t) (snd t)) xys |> List.max

let inline charToInt (c: char) = (int c - int '0')

let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList
let parsed = List.map (Seq.toList >> List.map charToInt) input

printfn $"Part 1: {checkPos parsed}"
printfn $"Part 2: {findMaxView parsed}"