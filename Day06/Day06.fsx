open System

let sample = "3,4,3,1,2"
let input = "4,3,3,5,4,1,2,1,3,1,1,1,1,1,2,4,1,3,3,1,1,1,1,2,3,1,1,1,4,1,1,2,1,2,2,1,1,1,1,1,5,1,1,2,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,5,1,4,2,1,1,2,1,3,1,1,2,2,1,1,1,1,1,1,1,1,1,1,4,1,3,2,2,3,1,1,1,4,1,1,1,1,5,1,1,1,5,1,1,3,1,1,2,4,1,1,3,2,4,1,1,1,1,1,5,5,1,1,1,1,1,1,4,1,1,1,3,2,1,1,5,1,1,1,1,1,1,1,5,4,1,5,1,3,4,1,1,1,1,2,1,2,1,1,1,2,2,1,2,3,5,1,1,1,1,3,5,1,1,1,2,1,1,4,1,1,5,1,4,1,2,1,3,1,5,1,4,3,1,3,2,1,1,1,2,2,1,1,1,1,4,5,1,1,1,1,1,3,1,3,4,1,1,4,1,1,3,1,3,1,1,4,5,4,3,2,5,1,1,1,1,1,1,2,1,5,2,5,3,1,1,1,1,1,3,1,1,1,1,5,1,2,1,2,1,1,1,1,2,1,1,1,1,1,1,1,3,3,1,1,5,1,3,5,5,1,1,1,2,1,2,1,5,1,1,1,1,2,1,1,1,2,1"

let read (input: string) =
    input.Split(',') 
    |> Array.map int

let solve fish steps =
    let rec loop steps (fishes: (int*bigint)[]) =
        let groupByTimer fishes =
            fishes
            |> Array.groupBy fst
            |> Array.map (fun (timer, counts) -> timer, counts |> Array.sumBy snd)

        if steps = 0 
        then fishes |> Array.sumBy snd
        else
            [| for (timer, count) in fishes do
                if timer = 0 
                then 
                    yield 6, count
                    yield 8, count
                else 
                    yield timer-1, count |]
            |> groupByTimer
            |> loop (steps-1)

    loop steps (fish |> Array.countBy id |> Array.map (fun (timer, count) -> timer, bigint count))

fsi.AddPrinter(fun (bi: bigint) -> $"{bi.ToString()}I")

solve (read sample) 80 // 5934
solve (read input) 80 // 387413

solve (read sample) 256 // 26984457539I
solve (read input) 256 // 1738377086345I

