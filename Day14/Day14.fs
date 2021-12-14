open System
open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

type Pair = char*char
type PairCounts = Map<Pair, bigint>

let read fileName =
    let parseInsertionRule (line : string) =
        let [| pair; insertion |] = line.Split (" -> ")
        (pair[0], pair[1]), insertion.[0]

    let lines = getPath fileName |> File.ReadAllLines
    let pairCounts =
        Array.ofSeq lines[0]
        |> Array.pairwise
        |> Array.countBy id
        |> Array.map (fun (pair, count) -> pair, bigint count)
        |> Map.ofArray

    let rules =
        lines[2..]
        |> Array.map parseInsertionRule
        |> Map.ofArray

    pairCounts, rules

let step (rules: Map<Pair, char>) (pairCounts: PairCounts) = 
    [| 
        for kv in pairCounts do
            let (c1, c2) = kv.Key
            let count = kv.Value
            let insertion = rules[c1, c2]
            yield (c1, insertion), count
            yield (insertion, c2), count
    |]
    |> Array.groupBy fst
    |> Array.map (fun (pair, counts) -> pair, counts |> Array.sumBy snd)
    |> Map.ofArray

let rec repeat n fn input = 
    if n = 0 
    then input
    else repeat (n-1) fn (fn input)

let countChars (pairCounts: PairCounts) =
    let addCharCount (c, count) charCounts  = 
        match charCounts |> Map.tryFind c with
        | None -> Map.add c count charCounts
        | Some existing -> Map.add c (count + existing) charCounts

    pairCounts
    |> Map.fold (fun charCounts (c1, c2) count -> 
        charCounts |> addCharCount (c1, count) |> addCharCount (c2, count)
    ) Map.empty
    |> Map.map (fun c count -> 
        if count % 2I = 0I 
        then count/2I
        else count/2I + 1I)

let solve count fileName =
    let pairCounts, rules = read fileName
    let pairCounts' = repeat count (step rules) pairCounts

    let charCounts = countChars pairCounts' |> Map.toArray

    let (maxChar, maxCount) = charCounts |> Array.maxBy snd
    let (minChar, minCount) = charCounts |> Array.minBy snd

    maxCount - minCount

#if INTERACTIVE
fsi.AddPrintTransformer(fun (chars: char[]) -> System.String chars)
fsi.AddPrintTransformer(fun (count: bigint) -> string count)
#endif

solve 10 "sample.txt" |> printfn "Sample 10: %A" // 1588
solve 10 "input.txt" |> printfn "Input 10: %A" // 3143

solve 40 "sample.txt" |> printfn "Sample 40: %A" // 2188189693529
solve 40 "input.txt" |> printfn "Input 40: %A" // 4110215602456
