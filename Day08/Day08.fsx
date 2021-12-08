open System.IO

let (</>) p1 p2 = Path.Combine(p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let parseLine (line: string) =
    let [| pattern; output |] = line.Split " | "
    let parseDigits (str: string) =
        str.Split ' ' 
        |> Array.map (Seq.sort >> Seq.toArray >> System.String)
        |> List.ofArray
    parseDigits pattern , parseDigits output

let read (fileName: string) =
    getPath fileName
    |> File.ReadAllLines
    |> Array.map parseLine

let getDigit (seg: string) =
    match seg.Length with
    | 2 -> Some 1
    | 4 -> Some 4
    | 3 -> Some 7
    | 7 -> Some 8
    | _ -> None

let solve1 fileName =
    read fileName
    |> Array.sumBy (fun (pattern, output) -> output |> List.choose getDigit |> List.length)

solve1 "sample.txt" // 26
solve1 "input.txt" // 495

let intersectCount seg str =
    Set.ofSeq str 
    |> Set.intersect (Set.ofSeq seg) 
    |> Seq.length

let getDigit' one four (str: string) =
    let intersect seg str =
        Set.ofSeq str 
        |> Set.intersect (Set.ofSeq seg) 
        |> Seq.length

    match str.Length, intersectCount one str, intersectCount four str with
    | 6, 2, 3 -> 0
    | 2, 2, 2 -> 1
    | 5, 1, 2 -> 2
    | 5, 2, 3 -> 3
    | 4, 2, 4 -> 4
    | 5, 1, 3 -> 5
    | 6, 1, 3 -> 6
    | 3, 2, 2 -> 7
    | 7, 2, 4 -> 8
    | 6, 2, 4 -> 9
    | _ -> failwith "Ooopsi"

fsi.AddPrintTransformer(fun (seg: Set<char>) -> seg |> Set.toArray |> System.String :> obj)

let solve2 (pattern: string list, output: string list) =
    let all = List.append pattern output
    let one = all |> List.find (fun str -> str.Length = 2)
    let four = all |> List.find (fun str -> str.Length = 4)

    let [ d1; d2; d3; d4 ] = output |> List.map (getDigit' one four)
    d1*1000 + d2*100 + d3*10 + d4

read "sample.txt" |> Array.map solve2 |> Array.sum // 61229
read "input.txt" |> Array.map solve2 |> Array.sum // 1055164
