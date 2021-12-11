open System
open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

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

module Char =
    let toInt (c : char) = int (String [| c |])

type 'a ``[,]`` with

    member arr.IsInRange (r, c) =
        r >= arr.GetLowerBound 0
        && r <= arr.GetUpperBound 0
        && c >= arr.GetLowerBound 1
        && c <= arr.GetUpperBound 1

    member arr.Range dimension =
        seq { arr.GetLowerBound dimension .. arr.GetUpperBound dimension }

    member arr.Indexes =
        [
            for row in arr.Range 0 do
                for col in arr.Range 1 do
                    row, col
        ]

    member arr.Neighbors (row, col) =
        List.allPairs [ -1 .. 1 ] [ -1 .. 1 ]
        |> List.filter (fun (row, col) -> (row, col) <> (0, 0))
        |> List.map (fun (offx, offy) -> row + offx, col + offy)
        |> List.filter arr.IsInRange

let step (octo : int [,]) =
    let mapIndexes f indexes =
        indexes
        |> List.iter (fun (row, col) -> octo.[row, col] <- f (octo.[row, col]))

    let isFlashing (row, col) = octo.[row, col] > 9

    let getFlashing () = octo.Indexes |> List.filter isFlashing

    let rec loop (toIncrement : (int * int) list) =
        if List.isEmpty toIncrement then
            let flashing = getFlashing ()

            flashing |> mapIndexes (fun _ -> 0)

            flashing.Length, octo
        else
            toIncrement |> mapIndexes (fun v -> v + 1)

            let newFlashes =
                toIncrement
                |> List.distinct
                |> List.filter isFlashing

            let toIncrement' =
                newFlashes
                |> List.collect octo.Neighbors
                |> List.filter (not << isFlashing)

            loop toIncrement'

    loop octo.Indexes

let read fileName =
    let lines = getPath fileName |> File.ReadAllLines

    Array2D.init lines.Length (lines.[0].Length) (fun row col -> Char.toInt (lines.[row].[col]))

let repeat n fn =
    List.init n (fun _ -> fn) |> List.reduce (>>)

let solve fileName count =
    let octo = read fileName

    let step' (flashes, octo) =
        let newFlashes, newOcto = step octo
        flashes + newFlashes, newOcto

    repeat count step' (0, octo) |> fst

solve "sample.txt" 100 |> printfn "%A" // 1656
solve "input.txt" 100 |> printfn "%A" // 1571

let solve2 fileName =
    let octo = read fileName

    let index =
        Seq.unfold (step >> Some) octo
        |> Seq.findIndex (fun flashes -> flashes = octo.Length)

    index + 1

solve2 "sample.txt" |> printfn "%A" // 195
solve2 "input.txt" |> printfn "%A" // 387
