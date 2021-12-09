open System
open System.IO

let (</>) p1 p2 = Path.Combine(p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

type 'a ``[,]`` with

    member arr.IsInRange (r, c) =
        r >= arr.GetLowerBound 0 
        && r <= arr.GetUpperBound 0 
        && c >= arr.GetLowerBound 1 
        && c <= arr.GetUpperBound 1

    member arr.Range dimension =
        seq { arr.GetLowerBound dimension .. arr.GetUpperBound dimension }

let read fileName =
    let lines =
        getPath fileName
        |> File.ReadAllLines
    let getValue row col =
        int ((lines[row][col]).ToString())
    Array2D.init lines.Length lines[0].Length getValue

let neighbors (arr: int[,]) (row, col) =
    let up = row-1, col
    let down = row+1, col
    let left = row, col-1
    let right = row, col+1
    [ up; down; left; right ]
    |> List.filter arr.IsInRange

let isLowPoint (arr: int[,]) (row, col) =
    let value = arr[row, col]
    neighbors arr (row, col) 
    |> List.forall (fun (r, c) -> arr[r, c] > value)

let lowPoints (arr: int[,]) =
    [ for row in arr.Range 0 do
        for col in arr.Range 1 do
            if isLowPoint arr (row, col) then
                yield row, col ]

let solve1 fileName =
    let arr = read fileName
    lowPoints arr
    |> List.map (fun (r, c) -> arr[r, c] + 1)
    |> List.sum

solve1 "sample.txt" // 15
solve1 "input.txt" // 530

let basin (arr: int[,]) lowPoint =
    let getNewEdge acc edge =
        edge
        |> Set.toList 
        |> List.collect (neighbors arr)
        |> List.filter (fun (row, col) -> arr[row, col] <> 9 && not (Set.contains (row, col) acc))
        |> Set.ofList

    let rec loop acc edge =
        if Set.isEmpty edge 
        then acc
        else
            let newEdge = getNewEdge acc edge
            loop (Set.union acc newEdge) newEdge
    
    let start = Set.singleton lowPoint
    loop start start

let solve2 fileName =
    let arr = read fileName
    lowPoints arr
    |> List.map (basin arr >> Set.count)
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)

solve2 "sample.txt" // 1134
solve2 "input.txt" // 1019494