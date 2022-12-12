open System.Collections.Generic

type Element =
    val X: int
    val Y: int
    val C: char
    val mutable Parent: Option<Element>
    new (x, y, c) = { X = x; Y = y; C = c; Parent = None }

let isCloseChar (a: Element) (b: Element) =
    let mutable (c1, c2) = (a.C, b.C)
    if   c1 = 'S' then c1 <- '`'
    elif c2 = 'E' then c2 <- '{'
    (int c2) - (int c1) <= 1

let neighbors (elems: Element list list) (e: Element) =
    let offs = [(-1, 0); (1, 0); (0, -1); (0, 1)]
    let (x, y) = (e.X, e.Y)
    let xys = List.map (fun (a, b) -> (x + a, y + b)) offs
    let xys = List.filter (fun (a, b) -> a >= 0 && b >= 0 && a < elems.[0].Length && b < elems.Length) xys
    let nElems =  List.map (fun (x, y) -> elems.[y].[x] ) xys
    List.filter  (fun (x: Element) -> isCloseChar e x ) nElems

let bfs input start =
    let q = Queue<Element>()
    let visited = HashSet<Element>([start])
    q.Enqueue start
    let mutable foundGoal = false
    let mutable goal = None
    while q.Count > 0 && (not foundGoal) do
        let curr = q.Dequeue()
        if curr.C = 'E' then
            foundGoal <- true
            goal <- Some curr
        else
            let ns = neighbors input curr
            ns |> List.iter (fun x ->
                if not (visited.Contains(x)) then
                    visited.Add(x) |> ignore
                    x.Parent <- Some(curr)
                    q.Enqueue(x)
            )
    goal

let input = 
            let cl = System.IO.File.ReadAllLines "input.txt"
                    |> Array.toList |> List.map (Seq.toList)
            let (x, y) = (cl.[0].Length, cl.Length)
            List.map (fun y -> List.map (fun x -> Element(x, y, cl.[y].[x])) [0..(x-1)]) [0..(y-1)]

let goal = bfs input (input.[20].[0])
let path = List.unfold (
                            fun x -> match x with
                                                     | Some (x: Element) -> Some(x, x.Parent)
                                                     | None -> None) goal
printfn "Part 1:  %d" (path.Length - 1)
printfn "Part 2: %d" (path.Length - 8) // solved visually