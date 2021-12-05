open System
open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let read fileName =
    let parseCoord (str: string) =
        let [|x; y|] = str.Split ","
        int x, int y
    let parseLine (line: string) =
        let  [| first; last |] = line.Split " -> "
        parseCoord first, parseCoord last

    getPath fileName
    |> File.ReadAllLines
    |> Array.map parseLine

let expand ((x1, y1), (x2, y2)) =
    let getAllBetween n1 n2 = 
        if n1 < n2 
        then [| n1..n2 |] 
        else  [| n1 .. -1 .. n2 |]

    if x1 = x2 
    then getAllBetween y1 y2 |> Array.map (fun y -> x1, y)
    elif y1 = y2
    then getAllBetween x1 x2 |> Array.map (fun x -> x, y1)
    else Array.zip (getAllBetween x1 x2) (getAllBetween y1 y2)

let isHorizontalOrVertical ((x1, y1), (x2, y2)) = x1 = x2 || y1 = y2
let isAny _ = true

let solve filter fileName =
    read fileName
    |> Array.filter filter
    |> Array.collect expand
    |> Array.countBy id
    |> Array.filter (fun (_, cnt) -> cnt > 1)
    |> Array.length

let show filter fileName =
    let counts =
        read fileName
        |> Array.filter filter
        |> Array.collect expand
        |> Array.countBy id
        |> Array.sortBy fst
        |> Map.ofArray

    for y in 0..9 do
        for x in 0..9 do
            match counts |> Map.tryFind (x, y) with
            | None -> printf "."
            | Some cnt -> printf "%d" cnt
        printfn ""

read "sample.txt"
|> Array.map expand
|> Array.iter (printfn "%A")

let solve1 fileName = solve isHorizontalOrVertical fileName

show isHorizontalOrVertical "sample.txt"
show isAny "sample.txt"

solve1 "sample.txt" // 5
solve1 "input.txt" // 5167

let solve2 fileName = solve isAny fileName

solve2 "sample.txt" // 12
solve2 "input.txt" // 17604

