open System

let sample = "3,4,3,1,2"
let input = "4,3,3,5,4,1,2,1,3,1,1,1,1,1,2,4,1,3,3,1,1,1,1,2,3,1,1,1,4,1,1,2,1,2,2,1,1,1,1,1,5,1,1,2,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,5,1,4,2,1,1,2,1,3,1,1,2,2,1,1,1,1,1,1,1,1,1,1,4,1,3,2,2,3,1,1,1,4,1,1,1,1,5,1,1,1,5,1,1,3,1,1,2,4,1,1,3,2,4,1,1,1,1,1,5,5,1,1,1,1,1,1,4,1,1,1,3,2,1,1,5,1,1,1,1,1,1,1,5,4,1,5,1,3,4,1,1,1,1,2,1,2,1,1,1,2,2,1,2,3,5,1,1,1,1,3,5,1,1,1,2,1,1,4,1,1,5,1,4,1,2,1,3,1,5,1,4,3,1,3,2,1,1,1,2,2,1,1,1,1,4,5,1,1,1,1,1,3,1,3,4,1,1,4,1,1,3,1,3,1,1,4,5,4,3,2,5,1,1,1,1,1,1,2,1,5,2,5,3,1,1,1,1,1,3,1,1,1,1,5,1,2,1,2,1,1,1,1,2,1,1,1,1,1,1,1,3,3,1,1,5,1,3,5,5,1,1,1,2,1,2,1,5,1,1,1,1,2,1,1,1,2,1"

let read (input: string) =
    input.Split(',') 
    |> Array.map int

let solve (input: int[]) (steps: int) =
    let mutable fishes = Array.zeroCreate 9
    for f in input do
        fishes.[f] <- fishes.[f] + 1
    for i in 1..steps do
        let zeroCount = fishes.[0]
        fishes <- Array.append fishes.[1..] [| zeroCount |] // create new lanterns
        fishes.[6] <- fishes.[6] + zeroCount // add lanternfish which created new ones
    Array.sum fishes

fsi.AddPrinter(fun (bi: bigint) -> $"{bi.ToString()}I")

solve (read sample) 80 // 5934
solve (read input) 80 // 387413

solve (read sample) 256 // 26984457539I
solve (read input) 256 // 1738377086345I



