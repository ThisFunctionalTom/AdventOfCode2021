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

let getSimpleDigit (seg: string) =
    match seg.Length with
    | 2 -> Some 1
    | 4 -> Some 4
    | 3 -> Some 7
    | 7 -> Some 8
    | _ -> None

let solve1 fileName =
    read fileName
    |> Array.sumBy (fun (_, output) -> output |> List.choose getSimpleDigit |> List.length)

solve1 "sample.txt" // 26
solve1 "input.txt" // 495

let getDigit one four (str: string) =
    let (|SegCount|_|) count (str: string) =
        if str.Length = count then Some SegCount else None

    let commonSegments s1 s2 =
        (Set.ofSeq s1, Set.ofSeq s2) 
        ||> Set.intersect 
        |> Seq.length

    let (|CommonWith|_|) pat count (str: string) =
        if commonSegments pat str = count then Some CommonWith else None
    let (|CommonWithOne|_|) = (|CommonWith|_|) one
    let (|CommonWithFour|_|) = (|CommonWith|_|) four

    match str with
    | SegCount 2 -> 1
    | SegCount 4 -> 4
    | SegCount 3 -> 7
    | SegCount 7 -> 8
    | SegCount 5 & CommonWithFour 2 -> 2
    | SegCount 5 & CommonWithOne 2 -> 3
    | SegCount 5 -> 5
    | SegCount 6 & CommonWithOne 1 -> 6
    | SegCount 6 & CommonWithFour 4 -> 9
    | SegCount 6 -> 0
    | _ -> failwith "Something went wrong."

let solve2 (pattern: string list, output: string list) =
    let all = List.append pattern output
    let one = all |> List.find (fun str -> str.Length = 2)
    let four = all |> List.find (fun str -> str.Length = 4)

    output 
    |> List.map (getDigit one four)
    |> List.fold (fun nr d -> nr*10 + d) 0

read "sample.txt" |> Array.map solve2 |> Array.sum // 61229
read "input.txt" |> Array.map solve2 |> Array.sum // 1055164