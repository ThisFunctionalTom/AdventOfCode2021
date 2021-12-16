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

    member arr.Neighbors (row, col) =
        [ row, col-1; row-1, col; row, col+1; row+1, col ]
        |> List.filter arr.IsInRange 
    
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

module String =
    let inline color colorNr txt = $"\u001b[{colorNr}m{txt}\u001b[0m"

    let inline black txt = color 30 txt
    let inline red txt = color 31 txt
    let inline green txt = color 32 txt
    let inline yellow txt = color 33 txt
    let inline blue txt = color 34 txt
    let inline magenta txt = color 35 txt
    let inline cyan txt = color 36 txt
    let inline white txt = color 37 txt

let maxPos lst = lst |> List.reduce (fun (r1, c1) (r2, c2) -> (max r1 r2), (max c1 c2)) 

let showRisks (risks: Map<Pos, int>) (changed: Pos list) =
    let (maxrow, maxcol) = risks |> Map.toList |> List.map fst |> maxPos
    [ for row in 0 .. maxrow do
        [ for col in 0 .. maxcol do
            let changed = changed |> List.contains (row, col)
            let value = risks |> Map.tryFind (row, col) |> Option.map (sprintf "%03d") |> Option.defaultValue " . "
            if changed then String.yellow value else value ]
        |> String.concat " " ] |> String.concat "\n"

let getRisksSeq (cave: Cave) =
    let getMinRisk risks (row, col) =
        let minNeighbor = 
            cave.Neighbors (row, col)
            |> List.choose (fun np -> Map.tryFind np risks)
            |> List.min
        minNeighbor + cave[row, col]

    let risksUnfolder ((risks: Map<Pos, int>), (changed: Pos list)) =
        //showRisks risks changed |> printfn "%s"
        let toUpdate = 
            changed 
            |> List.collect cave.Neighbors
            |> List.distinct
            |> List.choose (fun (row, col) ->
                let newMin = getMinRisk risks (row, col)
                match Map.tryFind (row, col) risks with
                | None -> Some ((row, col), newMin)
                | Some currRisk when currRisk > newMin -> Some ((row, col), newMin)
                | _ -> None)
        if List.isEmpty toUpdate 
        then None
        else
            let risks' = toUpdate |> List.fold (fun risks (pos, risk) -> Map.add pos risk risks) risks
            Some (risks', (risks', (toUpdate |> List.map fst)))

    Seq.unfold risksUnfolder ((Map.ofList [ (0, 0), cave[0, 0]]), [0, 0])

let solveCave cave =
    let risks = getRisksSeq cave |> Seq.last
    risks[(cave.Bottom, cave.Right)] - cave[0, 0]

let solve = read >> solveCave

solve "sample.txt" |> printfn "Part1 sample: %d" // 40
solve "input.txt" |> printfn "Part1 input: %d" // 390

let read2 fileName =
    let cave = read fileName
    Array2D.init (cave.Rows*5) (cave.Cols*5) <| fun row col ->
        let toAdd = col / cave.Cols + row / cave.Rows
        let value = cave[row%cave.Rows, col%cave.Cols]
        if value + toAdd > 9 then value + toAdd - 9 else value + toAdd        

let solve2 = read2 >> solveCave

solve2 "sample.txt" |> printfn "Part2 sample: %d" // 315
solve2 "input.txt" |> printfn "Part2 input: %d" // 2814 - 30.813s