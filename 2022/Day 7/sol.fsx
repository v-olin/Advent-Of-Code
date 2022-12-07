type File =
    val Name: string
    val Size: int
    new(name: string, size: int) = { Name = name; Size = size }

type Folder =
    val Name: string
    val Parent: Option<Folder>
    val mutable Files: List<File>
    val mutable Folders: List<Folder>
    new(name: string, parent: Option<Folder>) =
        { Name = name; Parent = parent; Files = []; Folders = [] }

let root = Folder("/", None)

let cd (name: string) (cwd: Folder) = 
    if name = "/" then root
    elif name = ".." then cwd.Parent.Value
    else List.find (fun x -> x.Name = name) cwd.Folders

let file (name: string) (size: int) (cwd: Folder) =
    cwd.Files <- File(name, size) :: cwd.Files
    cwd

let dir (name: string) (cwd: Folder) =
    cwd.Folders <- Folder(name, Some(cwd)) :: cwd.Folders
    cwd

let rec flatten (cwd: Folder) =
    if cwd.Folders.Length = 0 then [cwd]
    else
        List.fold (fun x y -> List.append x (flatten y)) [cwd] cwd.Folders

let read (i: string) (cwd: Folder) =
    let instr = i.Split(' ')
    if instr.Length = 2 then
        match instr.[0] with
        | "$" -> cwd
        | "dir" -> dir instr.[1] cwd
        | _ -> file instr.[1] (int instr.[0]) cwd
    else cd instr.[2] cwd

let rec sumDir (cwd: Folder) =
    let files = cwd.Files |> List.map (fun x -> x.Size) |> List.sum
    let folders = cwd.Folders |> List.map (fun x -> sumDir x) |> List.sum
    files + folders

let sumDirIncl = flatten >> List.map (fun x -> sumDir x)
                >> List.filter (fun x -> x < 100_000) >> List.sum

let findDirIncl (cwd: Folder) =
    let flattened = flatten cwd |> List.map (fun x -> sumDir x)
    let required = flattened.[0] - 40_000_000
    flattened |> List.filter (fun x -> x > required) |> Seq.min

let input = System.IO.File.ReadAllLines "input.txt"
input |> Array.fold (fun x y -> read y x) root |> ignore
printfn "Part 1: %d\nPart2: %d" (sumDirIncl root) (findDirIncl root)