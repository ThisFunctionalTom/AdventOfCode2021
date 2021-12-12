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

module Path =
    let isComplete =
        function
        | "end" :: _ -> true
        | _ -> false

    let show = List.rev >> String.concat "->"

#if INTERACTIVE
fsi.AddPrinter Path.show
#endif

let step (map : Map<Cave, Cave list>) (path : Path) =
    match path with
    | [] -> failwith "Should never happen"
    | cave :: _ ->
        map
        |> Map.find cave
        |> List.filter (fun c ->
            let doubleVisit =
                c.ToLowerInvariant () = c
                && path |> List.contains c

            not doubleVisit
        )
        |> List.map (fun c -> c :: path)

let solve fileName =
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

solve "sample.txt" |> printfn "%d" // 10
solve "sample2.txt" |> printfn "%d" // 19
solve "sample3.txt" |> printfn "%d" // 226
solve "input.txt" |> printfn "%d" // 5212
