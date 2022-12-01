let splitBy v list =
  let yieldRevNonEmpty list = 
    if list = [] then []
    else [List.rev list]

  let rec loop groupSoFar list = seq { 
    match list with
    | [] -> yield! yieldRevNonEmpty groupSoFar
    | head::tail when head = v ->
        yield! yieldRevNonEmpty groupSoFar
        yield! loop [] tail
    | head::tail ->
        yield! loop (head::groupSoFar) tail }
  loop [] list |> List.ofSeq

let sumOf list =
    list |> List.map int |> List.sum

let chunks = System.IO.File.ReadAllLines "input.txt"
            |> Array.toList
            |> splitBy ""
            |> List.map sumOf
            |> List.sortDescending
printfn "Part 1: %d" chunks.Head
printfn "Part 2: %d" (List.take 3 chunks |> List.sum)