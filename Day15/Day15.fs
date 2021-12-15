open System
open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

type Pos = (int*int)
type Path = Pos list
type Cave = int[,]

module Char =
    let toInt (c : char) = int $"{c}"

type 'a ``[,]`` with

    member arr.Rows = arr.GetUpperBound 0 - arr.GetLowerBound 0 + 1
    member arr.Cols = arr.GetUpperBound 1 - arr.GetLowerBound 1 + 1
    member arr.Bottom = arr.GetUpperBound 0
    member arr.Right = arr.GetUpperBound 1
    
    member arr.IsInRange (r, c) =
        r >= arr.GetLowerBound 0 
        && r <= arr.GetUpperBound 0 
        && c >= arr.GetLowerBound 1 
        && c <= arr.GetUpperBound 1

let read fileName =
    let cave =
        getPath fileName
        |> File.ReadAllLines
        |> Array.map (Array.ofSeq >> Array.map Char.toInt)

    Array2D.init cave.Length cave[0].Length (fun row col -> cave[row][col])

let endPos (cave: Cave) =
    (cave.GetUpperBound 0)-1, (cave.GetUpperBound 1)-1

let moves (cave: int[,]) (row, col) =
    let up = row-1, col
    let down = row+1, col
    let left = row, col-1
    let right = row, col+1
    [ up; down; left; right ]
    |> List.filter cave.IsInRange    

let nextMoves (cave: int[,]) (path: Path) : Pos list =
    match path with
    | lastPos :: _ ->
        moves cave lastPos
        |> List.filter (fun newPos -> not (List.contains newPos path))
    | [] -> failwith "This is not possible"

let endsWith (pos: Pos) (path: Path) =
    match path with
    | endPos :: _ -> endPos = pos
    | _ -> false

let nextMoves' (row, col) =
    [ if row > 0 then row-1, col 
      if col > 0 then row, col-1 ]

let getMinRiskPath (cave: Cave) =
    let rec loop minRisks =
        let next =
            minRisks 
            |> List.collect (fun (pos, risk) -> 
                nextMoves' pos
                |> List.map (fun (r, c) -> (r, c), risk + cave[r, c]))
            |> List.groupBy fst
            |> List.map (fun (pos, risks) -> pos, risks |> List.map snd |> List.min)
        match next with
        | [ (0, 0), totalRisk ] -> totalRisk - cave[0, 0]
        | _ -> loop next

    loop [ (cave.Bottom, cave.Right), cave[cave.Bottom, cave.Right] ]

let solve fileName =
    read fileName |> getMinRiskPath

solve "sample.txt" // 40
solve "input.txt" // 390

let read2 fileName =
    let cave = read fileName
    Array2D.init (cave.Rows*5) (cave.Cols*5) <| fun row col ->
        let toAdd = col / cave.Cols + row / cave.Rows
        let value = cave[row%cave.Rows, col%cave.Cols]
        if value + toAdd > 9 then value + toAdd - 9 else value + toAdd        

let solve2 fileName =
    read2 fileName |> getMinRiskPath

#if INTERACTIVE
fsi.AddPrintTransformer(fun (cave: int[,]) ->
    [ for row in cave.GetLowerBound 0 .. cave.GetUpperBound 0 do
        cave.[row, *] |> Array.map string |> String.concat "" ] |> String.concat "\n" :> obj)
#endif

solve2 "sample.txt" // 315
solve2 "input.txt" // 2822