open System
open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let read fileName =
    getPath fileName
    |> File.ReadAllLines

let countOnes (numbers: string[]) (pos: int) =
    numbers
    |> Array.sumBy (fun number -> if number.[pos] = '1' then 1 else 0)

let toInt (number: string) = Convert.ToInt32(number, 2)

let (|MostOnes|MostZeros|Equal|) (numbersCount, onesCount) =
    if 2*onesCount = numbersCount 
    then Equal
    elif 2*onesCount > numbersCount
    then MostOnes
    else MostZeros

let mostCommonBit = function
    | MostOnes -> '1'
    | MostZeros -> '0'
    | Equal -> failwith "equal count of zeros and ones"

let leastCommonBit = function
    | MostOnes -> '0'
    | MostZeros -> '1'
    | Equal -> failwith "equal count of zeros and ones"

let solve fileName =
    let numbers = read fileName

    let calcRate (getBit: (int*int -> char)) =
        [| for bitPos in 0..numbers.[0].Length-1 do
            getBit (numbers.Length, (countOnes numbers bitPos)) |]
        |> String
    ()

    let gammaRate = calcRate mostCommonBit
    let epsilonRate = calcRate leastCommonBit
    (toInt gammaRate) * (toInt epsilonRate)

solve "sample.txt" // 198
solve "input.txt" // 2250414

let someRating ratingFilter (numbers: string[]) =
    let filter bitPos (bitValue: char) (numbers: string[]) =
        numbers
        |> Array.filter (fun number -> number.[bitPos] = bitValue)
    let rec loop (numbers: string[]) (bitPos: int) =
        let bitValue = ratingFilter (numbers.Length, countOnes numbers bitPos)
        let numbers' = filter bitPos bitValue numbers
        if numbers'.Length = 1 
        then numbers'.[0]
        else loop numbers' (bitPos+1)

    loop numbers 0

let oxygenGeneratorRating (numbers: string[]) =
    let getFilterValue = function
        | MostOnes -> '1'
        | MostZeros -> '0'
        | Equal -> '1'
    someRating getFilterValue numbers

let co2ScrubberRating (numbers: string[]) =
    let getFilterValue = function
        | MostOnes -> '0'
        | MostZeros -> '1'
        | Equal -> '0'
    someRating getFilterValue numbers

let solve2 fileName =
    let numbers = read fileName
    let ogr = oxygenGeneratorRating numbers
    let csr = co2ScrubberRating numbers
    (toInt ogr) * (toInt csr)

solve2 "sample.txt" // 230
solve2 "input.txt" // 6085575
