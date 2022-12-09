type State =
    val mutable Knots: (int * int) list
    val mutable Visited: Set<(int * int)>
    new (k) = { Knots = k; Visited = Set.empty}

let getState (n: int) = State(List.replicate n (0, 0))

let inline signum (a: int) =
    if a < 0 then -1
    elif a = 0 then 0
    else 1

let inline dist (ax, ay) (bx, by) =
    sqrt (float (ax - bx) ** 2.0 + float (ay - by) ** 2.0)

let extend (s: string) =
    let s' = s.Split ' '
    List.replicate (int s'.[1]) s'.[0]

let inline dirTup (d: string) =
    match d with
    | "R" -> (1, 0)
    | "U" -> (0, 1)
    | "L" -> (-1, 0)
    | _ ->  (0, -1)

let moveHead (st: State) (d: string) =
    let (dx, dy) = dirTup d
    let mutable (hx, hy) = st.Knots.Head
    hx <- hx + dx
    hy <- hy + dy
    st.Knots <- List.updateAt 0 (hx, hy) st.Knots
    st

let moveTail (st: State) (i: int) =
    let (hx, hy) = st.Knots.[i-1]
    let (tx, ty) = st.Knots.[i]
    if (dist (hx, hy) (tx, ty)) < float 2 then st
    else
        let (dx, dy) = (signum (hx - tx), signum (hy - ty))
        st.Knots <- List.updateAt i (tx + dx, ty + dy) st.Knots
        if i = st.Knots.Length-1 then
            st.Visited <- Set.add (tx + dx, ty + dy) st.Visited
        st

let move (st: State) (d: string) =
    let st' = moveHead st d
    List.fold (fun x y -> moveTail x y) st' [1..st'.Knots.Length-1]

let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList |> List.map extend |> List.concat

let solve (st: State) (n: int) = List.fold (fun s x -> move s x) st input
                                |> fun x -> x.Visited
                                |> Set.add (0,0)
                                |> Set.count
                                |> printfn "Part %d: %d" n

solve (getState 2) 1
solve (getState 10) 2