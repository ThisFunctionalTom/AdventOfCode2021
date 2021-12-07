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

let getTotalFuel (getCrabFuel: int -> int) (crabs: int[]) =
    crabs |> Array.sumBy getCrabFuel

let solve (getFuel: int -> int -> int) fileName =
    let crabs = read fileName
    let p1 = Array.min crabs
    let p2 = Array.max crabs

    List.min [ for p in p1 .. p2 do getTotalFuel (getFuel p) crabs ] 

solve getFuel1 "sample.txt" // 37
solve getFuel1 "input.txt" // 355592

solve getFuel2 "sample.txt" // 168
solve getFuel2 "input.txt" // 101618069 

let crabs = read "input.txt"
let crabs = read "sample.txt"

module Array =
    let median arr = Array.sort arr |> Array.item (Array.length arr / 2)
    let mean arr = Array.sum arr / Array.length arr

let findMinFuel getStart getFuel crabs =
    let getTotalFor p =
        getTotalFuel (getFuel p) crabs, p
    
    let rec loop count (tf, p) =
        let (tf', p') = min (getTotalFor (p+1)) (getTotalFor (p-1))
        ()
        if tf' < tf 
        then loop (count+1) (tf', p') 
        else tf, p, count
    getStart crabs
    |> getTotalFor
    |> loop 0

[ for file in [ "sample.txt", "input.txt" ] do
    file, findMinFuel Array.median getFuel1 (read file)
    file, findMinFuel Array.mean getFuel2 (read file) ]

let median = Array.sort crabs |> Array.item (Array.length crabs / 2)

let smaller, bigger = crabs |> Array.partition (fun cp -> cp < mean)

[ for p in Array.max smaller .. Array.min bigger do
    getTotalFuel (getFuel2 p) crabs ]

smaller |> Array.sumBy (getFuel2 347)
smaller |> Array.sumBy (getFuel2 349)
bigger |> Array.sumBy (getFuel2 347)
bigger |> Array.sumBy (getFuel2 349)

