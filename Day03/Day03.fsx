open System
open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

let read fileName =
    getPath fileName
    |> File.ReadAllLines
    |> Array.map (Seq.map (string >> int) >> Array.ofSeq)

let countOnes numbers =
    let sumItem (x, y) = x + y
    let sumRows row1 row2 =
        Array.zip row1 row2
        |> Array.map sumItem
    numbers
    |> Array.reduce sumRows

let bitsToNumber (bits: int[]) =
    bits 
    |> Array.fold (fun acc bit -> acc <<< 1 ||| bit) 0

let (|MostOnes|MostZeros|Equal|) (numbersCount, onesCount) =
    if 2*onesCount = numbersCount 
    then Equal
    elif 2*onesCount > numbersCount
    then MostOnes
    else MostZeros

let mostCommonBit numbersCount onesCount =
    match numbersCount, onesCount with
    | MostOnes -> 1
    | MostZeros -> 0
    | Equal -> failwith "equal count of zeros and ones"

let leastCommonBit numbersCount onesCount =
    match numbersCount, onesCount with
    | MostOnes -> 0
    | MostZeros -> 1
    | Equal -> failwith "equal count of zeros and ones"

let solve fileName =
    let numbers = read fileName
    let onesCount = countOnes numbers

    let toNumber predicate = 
        onesCount 
        |> Array.map predicate 
        |> bitsToNumber
    
    let gammaRate = toNumber (mostCommonBit numbers.Length)
    let epsilonRate = toNumber (leastCommonBit numbers.Length)
    gammaRate * epsilonRate

solve "sample.txt" // 198
solve "input.txt" // 2250414

let someRating ratingFilter (numbers: int[][]) =
    let filter bitPos bitValue (numbers: int[][]) =
        numbers
        |> Array.filter (fun number -> number.[bitPos] = bitValue)
    
    let rec loop numbers bitPos =
        let onesCount = countOnes numbers
        let bitValue = ratingFilter (numbers.Length, onesCount.[bitPos])

        let numbers' = filter bitPos bitValue numbers
        if numbers'.Length = 1 
        then numbers'.[0]
        else loop numbers' (bitPos+1)

    loop numbers 0

let oxygenGeneratorRating (numbers: int[][]) =
    let getFilterValue (numbersCount, onesCount) =
        match numbers.Length, onesCount with
        | MostOnes -> 1
        | MostZeros -> 0
        | Equal -> 1
    someRating getFilterValue numbers

let co2ScrubberRating (numbers: int[][]) =
    let getFilterValue (numbersCount, onesCount) =
        match numbers.Length, onesCount with
        | MostOnes -> 0
        | MostZeros -> 1
        | Equal -> 0
    someRating getFilterValue numbers

let solve2 fileName =
    let numbers = read fileName
    let ogr = oxygenGeneratorRating numbers
    let csr = co2ScrubberRating numbers
    (bitsToNumber ogr), (bitsToNumber csr)

solve2 "sample.txt"
