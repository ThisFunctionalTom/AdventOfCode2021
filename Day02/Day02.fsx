open System.IO

let (</>) p1 p2 = Path.Combine (p1, p2)
let getPath fileName = __SOURCE_DIRECTORY__ </> fileName

type Position = 
    {
        Horizontal: int
        Depth: int
    }
    static member Zero = { Horizontal = 0; Depth = 0 }

module Position =
    let toResult pos = pos.Horizontal * pos.Depth

    let move pos (line: string) =
        let [|cmd; value|] = line.Split(" ")
        let value = int value
        match cmd with
        | "forward" -> { pos with Horizontal = pos.Horizontal + value }
        | "down" -> { pos with Depth = pos.Depth + value }
        | "up" -> { pos with Depth = pos.Depth - value }

let solve1 fileName =
    getPath fileName
    |> File.ReadAllLines
    |> Array.fold Position.move Position.Zero
    |> Position.toResult

solve1 "sample.txt"
solve1 "input.txt"

type PositionWithAim = 
    {
        Horizontal: int
        Depth: int
        Aim: int
    }
    static member Zero = { Horizontal = 0; Depth = 0; Aim = 0 }

module PositionWithAim =
    let toResult pos = pos.Horizontal * pos.Depth

    let move (pos: PositionWithAim) (line: string) =
        let [|cmd; value|] = line.Split(" ")
        let value = int value
        match cmd with
        | "forward" -> { pos with Horizontal = pos.Horizontal + value; Depth = pos.Depth + (pos.Aim * value) }
        | "down" -> { pos with Aim = pos.Aim + value }
        | "up" -> { pos with Aim = pos.Aim - value }    

let solve2 fileName =
    getPath fileName
    |> File.ReadAllLines
    |> Array.fold PositionWithAim.move PositionWithAim.Zero
    |> PositionWithAim.toResult

solve2 "sample.txt"
solve2 "input.txt"