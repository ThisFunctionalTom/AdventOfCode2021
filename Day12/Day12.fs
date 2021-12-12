open System
open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

type Cave = string
type Path = Cave list

let read fileName : Map<Cave, Cave list> =
    getPath fileName
    |> File.ReadAllLines
    |> Array.collect (fun line ->
        let [| cave1; cave2 |] = line.Split "-"
        [| cave1, cave2; cave2, cave1 |]
    )
    |> Array.groupBy fst
    |> Array.map (fun (fromCave, lst) -> fromCave, lst |> Array.map snd |> Array.toList)
    |> Map.ofArray

let (|Start|End|Small|Big|) =
    function
    | "start" -> Start
    | "end" -> End
    | cave when cave.ToLowerInvariant () = cave -> Small
    | _ -> Big

module Path =
    let isComplete =
        function
        | "end" :: _ -> true
        | _ -> false

    let show = List.rev >> String.concat "->"

    let isValid1 (path : Path) =
        path
        |> List.countBy id
        |> List.forall (
            function
            | Start, cnt -> cnt = 1
            | End, cnt -> cnt = 1
            | Small, cnt -> cnt = 1
            | Big, _ -> true
        )

    let isValid2 (path : Path) =
        let caveCounts = path |> List.countBy id

        let isSmall =
            function
            | Small -> true
            | _ -> false

        let smallCaveCounts =
            caveCounts |> List.filter (fst >> isSmall)

        let caveCountsOk =
            caveCounts
            |> List.forall (
                function
                | Start, cnt -> cnt = 1
                | End, cnt -> cnt = 1
                | Small, cnt -> cnt <= 2
                | Big, _ -> true
            )

        let smallCaveCountsOk =
            (smallCaveCounts |> List.sumBy snd)
            <= smallCaveCounts.Length + 1

        caveCountsOk && smallCaveCountsOk

#if INTERACTIVE
fsi.AddPrinter Path.show
#endif

let step isPathValid (map : Map<Cave, Cave list>) (path : Path) =
    match path with
    | [] -> failwith "Should never happen"
    | cave :: _ ->
        map
        |> Map.find cave
        |> List.map (fun c -> c :: path)
        |> List.filter isPathValid

let solve step fileName =
    let map = read fileName

    let rec loop complete incomplete =
        if Set.isEmpty incomplete then
            complete
        else
            let complete', incomplete' =
                incomplete
                |> List.ofSeq
                |> List.collect (fun path -> step map path)
                |> List.partition Path.isComplete
                |> fun (c, i) -> Set.ofList c, Set.ofList i

            loop (Set.union complete complete') incomplete'

    loop Set.empty (Set.singleton [ "start" ])
    |> Set.count

let solve1 = solve (step Path.isValid1)

solve1 "sample.txt" |> printfn "%d" // 10
solve1 "sample2.txt" |> printfn "%d" // 19
solve1 "sample3.txt" |> printfn "%d" // 226
solve1 "input.txt" |> printfn "%d" // 5212

let solve2 = solve (step Path.isValid2)

solve2 "sample.txt" |> printfn "%d" // 36
solve2 "sample2.txt" |> printfn "%d" // 103
solve2 "sample3.txt" |> printfn "%d" // 3509
solve2 "input.txt" |> printfn "%d" // 134862
