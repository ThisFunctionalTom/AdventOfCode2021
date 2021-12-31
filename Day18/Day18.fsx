open System
open System.IO

let (</>) p1 p2 = Path.Combine(p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

type SnailNumber =
    | Value of int
    | Pair of left: SnailNumber * right: SnailNumber

let rec toString (n: SnailNumber) =
    match n with
    | Value value -> $"{value}"
    | Pair (left, right) -> $"[{toString left},{toString right}]"

fsi.AddPrintTransformer(fun n -> toString n)

let parse (str: string) =
    let rec parseNumber (str: string) idx =
        match str.[idx] with
        | '[' ->
            let left, nextIdx = parseNumber str (idx + 1)

            if str.[nextIdx] <> ',' then
                failwith $"',' expected but found {str.[nextIdx]} at pos {nextIdx}"

            let right, nextIdx = parseNumber str (nextIdx + 1)

            if str.[nextIdx] <> ']' then
                failwith $"']' expected but found {str.[nextIdx]} at pos {nextIdx}"

            Pair(left, right), nextIdx + 1
        | c when Char.IsDigit c ->
            try
                let value = String([| c |]) |> int
                Value value, idx + 1
            with
            | _ -> failwith $"expected digit but found {c} at pos {idx}"
        | c -> failwith $"'[' expected but found {c} at pos {idx}"

    let nr, idx = parseNumber str 0

    if idx < str.Length then
        failwith $"There is a rest after parse: '{str.[idx..]}'"

    nr

let rec (|Explosion|_|) (level: int) (nr: SnailNumber) =
    let rec addToFirst toAdd =
        function
        | Value value -> Value(value + toAdd)
        | Pair (Value left, right) -> Pair(Value(left + toAdd), right)
        | Pair (left, right) -> Pair(addToFirst toAdd left, right)

    let rec addToLast toAdd =
        function
        | Value value -> Value(value + toAdd)
        | Pair (left, Value right) -> Pair(left, Value(right + toAdd))
        | Pair (left, right) -> Pair(left, addToLast toAdd right)

    let nextLevel = level + 1
    //printfn $"Level: {level}: {toString nr}"

    match nr with
    | Value _ -> None
    | Pair (Value left, Value right) when level > 4 -> Some(Value 0, left, right)
    | Pair (Value _, Value _) -> None
    | Pair (Explosion nextLevel (explodedLeft, addLeft, addRight), right) ->
        Some(Pair(explodedLeft, addToFirst addRight right), addLeft, 0)
    | Pair (left, Explosion nextLevel (explodedRight, addLeft, addRight)) ->
        Some(Pair(addToLast addLeft left, explodedRight), 0, addRight)
    | Pair (_, _) -> None

let tryExplode =
    function
    | Explosion 1 (explodedNr, _, _) -> Some explodedNr
    | _ -> None

let rec (|Split|_|) (nr: SnailNumber) =
    match nr with
    | Value value when value >= 10 ->
        let left = value / 2
        let right = value / 2 + value % 2
        Some(Pair(Value left, Value right))
    | Value _lessThen10 -> None
    | Pair (Split left, right) -> Some(Pair(left, right))
    | Pair (left, Split right) -> Some(Pair(left, right))
    | Pair (_noSplitLeft, _noSplitRight) -> None

let trySplit = (|Split|_|)

let reduce (nr: SnailNumber) =
    let rec loop (nr: SnailNumber) =
        match tryExplode nr
              |> Option.orElseWith (fun _ -> trySplit nr)
            with
        | Some reducedNr ->
            //printfn $"{toString reducedNr}"
            loop reducedNr
        | None -> nr

    //printfn $"{toString nr}"
    loop nr

let rec magnitude =
    function
    | Value value -> value
    | Pair (left, right) -> 3 * (magnitude left) + 2 * (magnitude right)

let add (nr1: SnailNumber) (nr2: SnailNumber) = Pair(nr1, nr2) |> reduce

let solve numbers =
    numbers |> Array.reduce add |> magnitude

let read fileName =
    getPath fileName
    |> File.ReadAllLines
    |> Array.map parse

read "sample.txt" |> solve // 4140
read "input.txt" |> solve // 4170
