open System
open System.IO
open System.Globalization

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

module rec Parser =
    let getBits =
        function
        | '0' -> "0000"
        | '1' -> "0001"
        | '2' -> "0010"
        | '3' -> "0011"
        | '4' -> "0100"
        | '5' -> "0101"
        | '6' -> "0110"
        | '7' -> "0111"
        | '8' -> "1000"
        | '9' -> "1001"
        | 'A' -> "1010"
        | 'B' -> "1011"
        | 'C' -> "1100"
        | 'D' -> "1101"
        | 'E' -> "1110"
        | 'F' -> "1111"

    let readBit = function
        | '1' -> 1
        | '0' -> 0
        | _ -> failwith "not a binary number"

    let toInt str = str |> Seq.fold (fun nr bit -> (nr <<< 1) ||| readBit bit) 0

    type Parser<'a> = string -> 'a*string

    let isEndOfStream : Parser<bool> =
        fun str -> str |> Seq.forall ((=) '0'), str

    let read n : Parser<_> =
        fun str -> str[..n-1], str[n..]

    let map f (p: Parser<_>) : Parser<_> =
        fun str ->
            let value, rest = p str
            f value, rest

    let bind (f: 'a -> Parser<'b>) (x: Parser<'a>) : Parser<'b> =
        fun str ->
            let value, rest = x str
            let p2 = f value
            p2 rest

    type ParserBuilder() =
        member _.Return (x: 'a) : Parser<'a> = fun str -> x, str
        member _.ReturnFrom f : Parser<'a> = f
        member _.Bind ((x: Parser<'a>), (f: 'a -> Parser<'b>)) : Parser<'b> = bind f x

    let parser = ParserBuilder()

    let pList count pItem =
        let rec loop items count  =
            parser {
                if count = 0 
                then return List.rev items
                else
                    let! nextItem = pItem
                    return! loop (nextItem :: items) (count - 1)
            }
        loop [] count
    
    let pMany pItem =
        let rec loop items =
            parser {
                let! isEnd = isEndOfStream
                if isEnd 
                then return List.rev items
                else
                    let! nextItem = pItem
                    return! loop (nextItem :: items)
            }
        loop []

    let pInt cnt = read cnt |> map toInt
    let pBool = read 1 |> map ((=) "1")

    let pVersion = pInt 3
    let pTypeId = pInt 3

    let pLiteral =
        let rec loop acc =
            parser {
                let! hasNextGroup = pBool
                let! groupValue = pInt 4
                let value = (acc <<< 4) ||| groupValue
                if hasNextGroup
                then return! loop value
                else return value
            }
        loop 0

    let pLengthPackets = 
        parser {
            let! length = pInt 15
            let! str = read length
            let packets, _ = pMany packet str
            return packets
        }

    let pCountPackets =
        parser {
            let! count = pInt 11
            return! pList count packet
        }

    let pOperator =
        parser {
            let! lengthTypeId = pInt 1
            return!
                if lengthTypeId = 0
                then pLengthPackets
                else pCountPackets
        }

    type Item =
    | Literal of int
    | Operator of (int*int*Item) list

    let packet = 
        parser {
            let! version = pVersion
            let! typeId = pTypeId

            let! content =
                match typeId with
                | 4 -> pLiteral |> map Literal
                | _ -> pOperator |> map Operator
            return version, typeId, content
        }

    let parse parser = Seq.map getBits >> String.concat "" >> parser

open Parser

let rec sumVersions = function
    | version, _, Literal _ -> version
    | v1, _, Operator items -> v1 + (items |> List.sumBy sumVersions)

let solve input =
    parse packet input |> fst |> sumVersions

parse packet "D2FE28" 
parse packet "38006F45291200" 
parse packet "EE00D40C823060" 

[ "8A004A801A8002F478"
  "620080001611562C8802118E34"
  "C0015000016115A2E0802F182340"
  "A0016C880162017C3686B18A3D4780" ] 
|> List.map solve
|> printfn "%A"

getPath "input.txt" |> File.ReadAllText |> solve |> printfn "%d" // 969



