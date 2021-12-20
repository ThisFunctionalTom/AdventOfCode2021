let stepX (vx, x) =
    let x' = x + vx

    let vx' =
        if vx < 0 then vx + 1
        elif vx > 0 then vx - 1
        else 0

    vx', x'

let stepY (vy, y) =
    let y' = y + vy
    let vy' = vy - 1
    vy', y'

let step ((vx, vy), (x, y)) =
    let vx', x' = stepX (vx, x)
    let vy', y' = stepY (vy, y)

    (vx', vy'), (x', y')

let steps step startVel startPos =
    Seq.unfold (fun (vel, pos) -> Some(pos, step (vel, pos))) (startVel, startPos)

let lessThan n = fun x -> x < n
let greaterThan n = fun x -> x > n
let missed (maxX, minY) (x, y) = x > maxX || y < minY

let inTargetArea (minX, maxX) (minY, maxY) (x, y) =
    x >= minX && x <= maxX && y >= minY && y <= maxY

let solve (minX, maxX) (minY, maxY) =
    let minVx = int (sqrt (2. * float minX)) - 1
    let maxVx = maxX + 1
    let maxVy = -minY

    [ for vx in minVx .. maxVx do
          for vy in maxVy .. -1 .. 0 do
              let shotTarget =
                  steps step (vx, vy) (0, 0)
                  |> Seq.takeWhile (not << missed (maxX, minY))
                  |> Seq.exists (inTargetArea (minX, maxX) (minY, maxY))

              let maxY = vy * (vy + 1) / 2
              if shotTarget then yield (vx, vy), maxY ]

// target area: x=96..125, y=-144..-98
solve (10, 30) (-10, -5)
|> List.map snd
|> List.max

solve (96, 125) (-144, -98)
|> List.map snd
|> List.max
