open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let read (fileName: string) = 
    let input = 
        getPath fileName
        |> File.ReadAllText
    
    input.Split "," 
    |> Array.map int

let getFuel1 pos crabPos =
    abs (crabPos - pos)

let getFuel2 pos crabPos =
    let n = abs (crabPos - pos)
    (n * (n+1)) / 2 // Array.sumBy [0..n]

let solve (getFuel: int -> int -> int) fileName =
    let crabs = read fileName
    let p1 = Array.min crabs
    let p2 = Array.max crabs

    [ for p in p1 .. p2 do
        crabs |> Array.sumBy (getFuel p) ] 
    |> List.min

solve getFuel1 "sample.txt" // 37
solve getFuel1 "input.txt" // 355592

solve getFuel2 "sample.txt" // 168
solve getFuel2 "input.txt" // 101618069 