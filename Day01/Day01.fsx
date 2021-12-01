open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let getInput fileName =
    getPath fileName
    |> File.ReadAllLines
    |> Array.map int

let countAscending (input: int[]) =
    Array.pairwise input
    |> Array.sumBy (fun (p, n) -> if n > p then 1 else 0)

getInput "sample.txt"
|> countAscending

getInput "input.txt"
|> countAscending

let countWindowedAscending (input: int[]) =
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> countAscending

getInput "sample.txt"
|> countWindowedAscending

getInput "input.txt"
|> countWindowedAscending

