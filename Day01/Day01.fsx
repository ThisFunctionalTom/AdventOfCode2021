open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let getInput fileName =
    getPath fileName
    |> File.ReadAllLines
    |> Array.map int

let countLarger (input: int[]) =
    input
    |> Array.pairwise
    |> Array.countBy (fun (p, n) -> n > p)
    |> Map.ofArray
    |> Map.find true

getInput "sample.txt"
|> countLarger

getInput "input.txt"
|> countLarger

let countWindowsLarger (input: int[]) =
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> countLarger

getInput "sample.txt"
|> countWindowsLarger

getInput "input.txt"
|> countWindowsLarger