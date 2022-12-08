let inline charToInt c = (int c - int '0')

let zipper (a: List<'T>) (b: List<'T>) =
    let zipper'  (b: List<'T>) (a: 'T) = List.map (fun x -> (a, x)) b
    List.map (zipper' b) a |> List.concat

let checkRow (m: List<List<int>>) (x: int) (y: int) =
    let row = m.[y]
    let me = m.[y].[x]
    let left = List.take x row |> List.map (fun x -> x < me) |> List.fold (&&) true
    let right = List.skip (x+1) row |> List.map (fun x -> x < me) |> List.fold (&&) true
    left || right

let checkCol (m: List<List<int>>) (x: int) (y: int) =
    let col = (List.transpose m).[x]
    let me = m.[y].[x]
    let up = List.take y col |> List.map (fun x -> x < me) |> List.fold (&&) true
    let down = List.skip (y+1) col |> List.map (fun x -> x < me) |> List.fold (&&) true
    up || down

let checkPos (m: List<List<int>>) =
    let xs = [1..(m.[0].Length)-2]
    let ys = [1..m.Length-2]
    let xys = zipper xs ys
    let horizontal = List.map (fun t -> checkRow m (fst t) (snd t)) xys
    let vertical = List.map (fun t -> checkCol m (fst t) (snd t)) xys
    let zipped = List.zip horizontal vertical
    let inner = List.map (fun t -> if (fst t) || (snd t) then 1 else 0) zipped |> List.sum
    let outer = (m.Length)*2 + (m.[0].Length)*2 - 4
    outer + inner

let rec foldWhile' (s: int) (h: int) (l: List<int>)  =
    if l.Length = 0 then s
    elif l.Head < h then (foldWhile' (s+1) h l.Tail)
    else s+1

let viewDistance (m: List<List<int>>) (x: int) (y: int) =
    let me = m.[y].[x]
    let left = List.take x m.[y] |> List.rev |> foldWhile' 0 me
    let right = List.skip (x+1) m.[y] |> foldWhile' 0 me
    let m' = List.transpose m
    let up = List.take y m'.[x] |> List.rev |> foldWhile' 0 me
    let down = List.skip (y+1) m'.[x] |> foldWhile' 0 me
    left * right * up * down

let findMaxView (m: List<List<int>>) =
    let xs = [1..(m.[0].Length)-2]
    let ys = [1..m.Length-2]
    let xys = zipper xs ys
    List.map (fun t -> viewDistance m (fst t) (snd t)) xys |> List.max

let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList
let parsed = List.map (Seq.toList >> List.map charToInt) input

printfn $"Part 1: {checkPos parsed}"
printfn $"Part 2: {findMaxView parsed}"