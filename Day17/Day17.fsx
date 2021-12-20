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

let doesHitTarget (minX, maxX) (minY, maxY) (vx, vy) =
    steps step (vx, vy) (0, 0)
    |> Seq.takeWhile (not << missed (maxX, minY))
    |> Seq.exists (inTargetArea (minX, maxX) (minY, maxY))

let listAll (minX, maxX) (minY, maxY) (minVx, maxVx) (minVy, maxVy) =
    [ for vx in minVx .. maxVx do
          for vy in minVy .. maxVy do
              let hitsTarget =
                  doesHitTarget (minX, maxX) (minY, maxY) (vx, vy)

              (vx, vy), hitsTarget ]

let solve (minX, maxX) (minY, maxY) =
    listAll (minX, maxX) (minY, maxY) (1, maxX + 1) (0, -(minY - 1))
    |> List.filter snd
    |> List.map (fun ((vx, vy), _) -> vy * (vy + 1) / 2)
    |> List.max

// target area: x=96..125, y=-144..-98
solve (20, 30) (-10, -5)
solve (96, 125) (-144, -98) // 10296

let solve2 (minX, maxX) (minY, maxY) =
    listAll (minX, maxX) (minY, maxY) (1, maxX + 1) (minY - 1, -(minY - 1))
    |> List.filter snd

solve2 (20, 30) (-10, -5) |> List.length
solve2 (96, 125) (-144, -98) |> List.length // 2371
