open System
open System.IO

let (</>) p1 p2 = Path.Combine(p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let read (fileName: string) =
    getPath fileName
    |> File.ReadAllLines

let (|Fine|Corrupted|Incomplete|) (line: string) =
    let rec loop openParens rest =
        match openParens, rest with
        | [], [] -> Fine
        | _, [] -> Incomplete openParens
        | '(' :: openParens', ')' :: rest' -> loop openParens' rest'
        | '[' :: openParens', ']' :: rest' -> loop openParens' rest'
        | '{' :: openParens', '}' :: rest' -> loop openParens' rest'
        | '<' :: openParens', '>' :: rest' -> loop openParens' rest'
        | o::_, (')' | ']' | '}' | '>' as c) :: rest' -> Corrupted (o, c)
        | _, ('(' | '[' | '{' | '<' as o) :: rest' -> loop (o::openParens) rest'

    loop [] (line |> List.ofSeq)

let scoreChecker (line: string) =
    match line with
    | Corrupted (o, ')') -> 3
    | Corrupted (o, ']') -> 57
    | Corrupted (o, '}') -> 1197
    | Corrupted (o, '>') -> 25137
    | Fine | Incomplete _ -> 0

read "sample.txt" |> Array.sumBy scoreChecker // 26397
read "input.txt" |> Array.sumBy scoreChecker // 265527

let scoreLineAutoComplete (openParens: char list) =
    let getCloseParen = function | '(' -> ')' | '[' -> ']' | '{' -> '}' | '<' -> '>'
    let scoreCloseParen = function | ')' -> 1UL | ']' -> 2UL | '}' -> 3UL | '>' -> 4UL
    let addScore score openParen = (getCloseParen openParen |> scoreCloseParen) + (5UL * score)
    openParens |> List.fold addScore 0UL

let scoreAutoComplete (lines: string[]) =
    let incompleteLines =
        lines
        |> Array.choose (function | Incomplete openParens -> Some openParens | _ -> None)
        |> Array.map scoreLineAutoComplete
    Array.sort incompleteLines
    |> Array.item (incompleteLines.Length/2)

read "sample.txt" |> scoreAutoComplete // 288957
read "input.txt" |> scoreAutoComplete // 3969823589UL