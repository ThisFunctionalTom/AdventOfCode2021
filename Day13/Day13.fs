#if INTERACTIVE
#load "../Extensions.fsx"
#endif

open System.Text
open System.IO
open Extensions

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

module Dots =
    let show (dots : Set<int * int>) =
        let (maxx, maxy) =
            dots
            |> Set.fold (fun (mx, my) (x, y) -> max mx x, max my y) (0, 0)

        let sb = StringBuilder ()

        for y in 0 .. maxy do
            for x in 0 .. maxx do
                if dots.Contains (x, y) then
                    sb.Append "#"
                else
                    sb.Append "."
                |> ignore

            sb.Append "\n" |> ignore

        printfn "%s" (sb.ToString ())

let parseDot (line : string) =
    let [| x; y |] = line.Split "," |> Array.map int
    x, y

let parseFold (line : string) =
    let [| axis; linePos |] =
        line.Substring("fold along ".Length).Split ("=")

    axis, int linePos

let read fileName =
    let [| dots; folds |] =
        getPath fileName
        |> File.ReadAllLines
        |> Array.split [| "" |]

    dots |> Array.map parseDot |> Set.ofArray, folds |> Array.map parseFold

let fold (dots : Set<int * int>) (coord : string, pos : int) =
    let foldVertically (x, y) =
        if y > pos then
            x, pos - (y - pos)
        else
            x, y

    let foldHorizontally (x, y) =
        if x > pos then
            pos - (x - pos), y
        else
            x, y

    let folder =
        match coord with
        | "x" -> foldHorizontally
        | "y" -> foldVertically

    dots |> Set.map folder

let solve1 fileName =
    let dots, folds = read fileName

    fold dots folds.[0] |> Set.count

let dots, folds = read "sample.txt"

solve1 "sample.txt" |> printfn "%A" // 17
solve1 "input.txt" |> printfn "%A" // 842

let solve2 fileName =
    let dots, folds = read fileName

    folds
    |> Array.fold (fun d f -> fold d f) dots
    |> Dots.show

solve2 "input.txt" // BFKRCJZU
