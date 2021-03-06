#load "Day18.fsx"

open Day18

let addDebug (nr1: SnailNumber) (nr2: SnailNumber) =
    let result = add nr1 nr2

    let debug =
        [ $"  {toString nr1}"
          $"+ {toString nr2}"
          $"= {toString result}" ]
        |> String.concat "\n"

    result, debug

let firstSamples =
    [ "[1,2]"
      "[[1,2],3]"
      "[9,[8,7]]"
      "[[1,9],[8,5]]"
      "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
      "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
      "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]" ]

let reductionSamples =
    [ "[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"
      "[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"
      "[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"
      "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
      "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]" ]

tryExplode (parse "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")

reductionSamples
|> List.map (fun (nr, expected) ->
    let nr = parse nr
    let expected = parse expected
    let actual = nr |> tryExplode |> Option.get

    if actual = expected then
        $"OK: {toString nr}"
    else
        $"Expected: {toString expected} but gut {toString actual}")

let nr1 = parse "[[[[4,3],4],4],[7,[[8,4],9]]]"
let nr2 = parse "[1,1]"

add nr1 nr2

let additionSample =
    [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
      "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
      "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
      "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
      "[7,[5,[[3,8],[1,4]]]]"
      "[[2,[2,2]],[8,[8,1]]]"
      "[2,9]"
      "[1,[[[9,3],9],[[9,0],[0,7]]]]"
      "[[[5,[7,4]],7],1]"
      "[[[[4,2],2],6],[8,7]]" ]

let additionOutput =
    """  [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
+ [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]

  [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
+ [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
= [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]

  [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
+ [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
= [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]

  [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
+ [7,[5,[[3,8],[1,4]]]]
= [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]

  [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
+ [[2,[2,2]],[8,[8,1]]]
= [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]

  [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
+ [2,9]
= [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]

  [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
+ [1,[[[9,3],9],[[9,0],[0,7]]]]
= [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]

  [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
+ [[[5,[7,4]],7],1]
= [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]

  [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
+ [[[[4,2],2],6],[8,7]]
= [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"""

let numbers = additionSample |> List.map parse

numbers.Length

numbers
|> List.skip 1
|> List.scan add numbers.[0]
|> List.pairwise
|> List.zip numbers.[1..]
|> List.map (fun (nr, (prev, result)) ->
    [ $"  {toString prev}"
      $"+ {toString nr}"
      $"= {toString result}" ]
    |> String.concat "\n")
|> String.concat "\n\n"
|> (=) additionOutput

let magnitudeSamples =
    [ "[[1,2],[[3,4],5]]", 143
      "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384
      "[[[[1,1],[2,2]],[3,3]],[4,4]]", 445
      "[[[[3,0],[5,3]],[4,4]],[5,5]]", 791
      "[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137
      "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488 ]

magnitudeSamples
|> List.filter (fun (nrStr, expected) ->
    let nr = parse nrStr
    let actual = magnitude nr
    actual <> expected)
