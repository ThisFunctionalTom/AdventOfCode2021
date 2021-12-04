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

let getWinningBoard boardResults =
    boardResults
    |> Array.minBy (fun (boardNr, drawIdx, board) -> drawIdx)

let getLoosingBoard boardResults =
    boardResults
    |> Array.maxBy (fun (boardNr, drawIdx, board) -> drawIdx)

let getBoardScore (winningDraws: int[]) (board: int[][]) =
    let unmarked =
        Array.concat board
        |> Array.filter (fun nr -> winningDraws |> Array.contains nr |> not)
    Array.sum unmarked * winningDraws.[^0]

let solve strategy fileName =
    let draw, boards = read fileName
    let boardNr, drawIdx, board =
        boards
        |> Array.mapi (fun boardIdx board -> boardIdx+1, getBoardWinIndex draw board, board)
        |> Array.map (fun (boardNr, drawIdx, board) -> 
            printfn $"boardNr: {boardNr}, drawIdx: {drawIdx}"
            boardNr, drawIdx, board)
        |> strategy
    boardNr, drawIdx+1, getBoardScore draw.[..drawIdx] board

solve getWinningBoard "sample.txt" // 4512
solve getWinningBoard "input.txt" // 21607

solve getLoosingBoard "sample.txt" // 1924
solve getLoosingBoard "input.txt" // 19012
