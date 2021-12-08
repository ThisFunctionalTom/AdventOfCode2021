open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let parseLine (line: string) =
    let [| pattern; output |] = line.Split " | "
    pattern.Split ' ', output.Split ' '

let read (fileName: string) = 
    getPath fileName
    |> File.ReadAllLines
    |> Array.map parseLine

let getDigit (str: string) =
    match str.Length with
    | 2 -> Some 1
    | 4 -> Some 4
    | 3 -> Some 7
    | 7 -> Some 8
    | _ -> None

let solve1 fileName =
    read fileName
    |> Array.sumBy (fun (pattern, output) -> 
        output 
        |> Array.choose getDigit
        |> Array.length)

solve1 "sample.txt"
solve1 "input.txt"