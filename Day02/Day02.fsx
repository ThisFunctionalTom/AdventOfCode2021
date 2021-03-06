open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let parseLine (line: string) = 
    let [|cmd; value|] = line.Split(" ")
    let value = int value
    cmd, value

let move1 (pos, depth) = function
    | "forward", value -> (pos + value, depth)
    | "down", value -> (pos, depth + value)
    | "up", value -> (pos, depth - value)

let readCommands fileName =
    getPath fileName
    |> File.ReadAllLines
    |> Array.map parseLine

let solve1 fileName =
    readCommands fileName
    |> Array.fold move1 (0, 0)
    |> fun (hor, depth) -> hor * depth

solve1 "sample.txt" // 150
solve1 "input.txt" // 2036120

let move2 (pos, depth, aim) = function
    | "forward", value -> (pos+value, depth+aim*value, aim)
    | "down", value -> (pos, depth, aim+value)
    | "up", value -> (pos, depth, aim-value)

let solve2 fileName =
    readCommands fileName
    |> Array.fold move2 (0, 0, 0)
    |> fun (hor, depth, aim) -> hor*depth

solve2 "sample.txt" // 900
solve2 "input.txt" // 2015547716