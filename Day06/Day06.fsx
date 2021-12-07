open System

let sample = "3,4,3,1,2"
let input = "4,3,3,5,4,1,2,1,3,1,1,1,1,1,2,4,1,3,3,1,1,1,1,2,3,1,1,1,4,1,1,2,1,2,2,1,1,1,1,1,5,1,1,2,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,5,1,4,2,1,1,2,1,3,1,1,2,2,1,1,1,1,1,1,1,1,1,1,4,1,3,2,2,3,1,1,1,4,1,1,1,1,5,1,1,1,5,1,1,3,1,1,2,4,1,1,3,2,4,1,1,1,1,1,5,5,1,1,1,1,1,1,4,1,1,1,3,2,1,1,5,1,1,1,1,1,1,1,5,4,1,5,1,3,4,1,1,1,1,2,1,2,1,1,1,2,2,1,2,3,5,1,1,1,1,3,5,1,1,1,2,1,1,4,1,1,5,1,4,1,2,1,3,1,5,1,4,3,1,3,2,1,1,1,2,2,1,1,1,1,4,5,1,1,1,1,1,3,1,3,4,1,1,4,1,1,3,1,3,1,1,4,5,4,3,2,5,1,1,1,1,1,1,2,1,5,2,5,3,1,1,1,1,1,3,1,1,1,1,5,1,2,1,2,1,1,1,1,2,1,1,1,1,1,1,1,3,3,1,1,5,1,3,5,5,1,1,1,2,1,2,1,5,1,1,1,1,2,1,1,1,2,1"

let read (input: string) =
    input.Split(',') 
    |> Array.map int

let solve (input: int[]) (steps: int) =
    let mutable fish = Array.zeroCreate 9
    for f in input do
        fish.[f] <- fish.[f] + 1I

    for i in 1..steps do
        fish <- 
            Array.init 9 <| function
            | 6 -> fish.[0] + fish.[6+1] 
            | 8 -> fish.[0] 
            | i -> fish.[i+1]
    Array.sum fish

fsi.AddPrinter(fun (bi: bigint) -> $"{bi.ToString()}I")

solve (read sample) 80 // 5934
solve (read input) 80 // 387413

solve (read sample) 256 // 26984457539I
solve (read input) 256 // 1738377086345I

solve' (read sample) 80 // 5934
solve' (read input) 80 // 387413

solve' (read sample) 256 // 26984457539I
solve' (read input) 256 // 1738377086345I



