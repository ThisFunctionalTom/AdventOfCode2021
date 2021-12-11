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

let showCell (row, col, value) = if value > 9 then "*" else string value

let showHighlightedCell highlighted color (row, col, value) =
    let str = showCell (row, col, value)

    if highlighted |> List.contains (row, col) then
        color str
    else
        str

let show (octo : int [,]) showCell =
    [
        for row in octo.Range 0 do
            octo.Range 1
            |> Seq.map (fun col -> showCell (row, col, octo.[row, col]))
            |> String.concat ""
    ]
    |> List.iter (printfn "%s")

let step (octo : int [,]) =
    printfn "%s" (String.blue $"************************************")

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
            printfn ""

            show
                octo
                (fun (row, col, value) ->
                    let str = if value > 9 then "*" else string value

                    if List.contains (row, col) toIncrement then
                        String.green str
                    elif str = "*" then
                        String.yellow str
                    else
                        str
                )

            toIncrement |> mapIndexes (fun v -> v + 1)

            let newFlashes = toIncrement |> List.filter isFlashing

            printfn ""
            show octo (showHighlightedCell newFlashes String.red)

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

    repeat count step' (0, octo)

solve "sample.txt" 2

let solve fileName count =
    let octo = read "sample.txt"

    let folder (flashes, octo) _ =
        let newFlashes, newOcto = step octo
        flashes + newFlashes, newOcto

    [ 1 .. count ] |> List.fold folder (0, octo)

[
    for coord in octo.Indexes do
        octo.Neighbors coord
]
|> List.countBy List.length
|> printfn "%A"

String.concat
    ""
    [
        String.red "Hello"
        "World"
        String.green "!"
    ]

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

let showCell (row, col, value) = if value > 9 then "*" else string value

let showHighlightedCell highlighted color (row, col, value) =
    let str = showCell (row, col, value)

    if highlighted |> List.contains (row, col) then
        color str
    else
        str

let show (octo : int [,]) showCell =
    [
        for row in octo.Range 0 do
            octo.Range 1
            |> Seq.map (fun col -> showCell (row, col, octo.[row, col]))
            |> String.concat ""
    ]
    |> List.iter (printfn "%s")

let step (octo : int [,]) =
    // printfn "%s" (String.blue $"************************************")

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

            // printfn ""

            // show
            //     octo
            //     (fun (row, col, value) ->
            //         let str = if value > 9 then "*" else string value

            //         if List.contains (row, col) newFlashes then
            //             String.red str
            //         elif List.contains (row, col) toIncrement' then
            //             String.green str
            //         elif str = "*" then
            //             String.yellow str
            //         else
            //             str
            //     )

            // toIncrement'
            // |> List.filter (fst >> (=) 8)
            // |> List.countBy id
            // |> List.iter (printfn "%A")

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
