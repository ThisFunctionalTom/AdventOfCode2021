open System
open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let read fileName =
    let parseRow (line: string) =
        line.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
    let lines =
        getPath fileName
        |> File.ReadAllLines

    let draw = lines.[0].Split(',') |> Array.map int
    let boards = 
        lines.[1..] 
        |> Array.chunkBySize 6 
        |> Array.map (Array.skip 1)
        |> Array.map (fun rows -> rows |> Array.map parseRow)
    draw, boards

let getLineWinIndex (draw: int[]) (line: int[]) =
    line 
    |> Array.map (fun nr -> draw |> Array.tryFindIndex ((=) nr) |> Option.defaultValue draw.Length) 
    |> Array.max 

let getBoardWinIndex (draw: int[]) (board: int[][]) =
    Array.append board (Array.transpose board)
    |> Array.map (fun row -> getLineWinIndex draw row) 
    |> Array.min

let getWinningBoard (draw: int[]) (boards: int[][][]) =
    boards
    |> Array.mapi (fun boardNr board -> boardNr, board, getBoardWinIndex draw board)
    |> Array.minBy (fun (_, _, winIdx) -> winIdx)

let solve fileName =
    let draw, boards = read fileName
    let boardNr, board, index = getWinningBoard draw boards

    let unmarked =
        let drawn = draw.[0..index]
        Array.concat board
        |> Array.filter (fun nr -> drawn |> Array.contains nr |> not)

    boardNr+1, index+1, Array.sum unmarked * draw.[index]

let draw, boards = read "input.txt"

boards
|> Array.map (getBoardWinIndex draw)
|> Array.countBy id
|> Array.sortBy fst

solve "sample.txt"
solve "input.txt"

let showBoard (board: string[][]) =
    for row in board do
        printfn ""
        for nr in row do
            printf "%3s" nr
    printfn ""
    printfn ""

showBoard b1
showBoard (Array.transpose b1)

Array.transpose b1