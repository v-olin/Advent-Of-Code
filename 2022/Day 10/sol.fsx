type State =
    val mutable History: List<int * int>
    new(history: List<int * int>) = { History = history }

let parse (s: string) =
    let s' = s.Split(' ')
    if s'.[0] = "noop" then (s'.[0], 0)
    else (s'.[0], int s'.[1])

let exec (st: State) (i: string * int) =
    let (cycle, value) = st.History.Head
    let (instr, arg) = i
    if instr = "noop" then
        st.History <- (cycle + 1, value) :: st.History
        st
    else
        let (proc: List<int * int>) = (cycle + 2, value + arg) :: [(cycle + 1, value)]
        st.History <- proc @ st.History
        st

let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList |> List.map parse
let m = List.fold (fun x y -> exec x y) (State([(0, 1)])) input
        |> fun x -> x.History
        |> Map.ofList

let drawRow (l: List<int * int>) =
    let drawPixel (x: int, y: int) =
        if abs ((x % 40) - y) < 2 then "#" else "."
    List.map drawPixel l |> String.concat ""

List.map (fun x -> m.[x-1] * x) [for i in 0..5 -> i*40 + 20]
    |> List.sum
    |> printfn "Part 1: %d"

printfn "Part 2:"
List.chunkBySize 40 (Map.toList m)
                    |> List.map (drawRow >> printfn "%s")