#load "monke.fs"


let input =
    "day-17.input"
    |> Monke.IO.readLines
    |> Seq.map Monke.String.chars


type Cube =
    { Cells: Map<uint, bool>
      StartX: int8
      EndX: int8
      StartY: int8
      EndY: int8
      StartZ: int8
      EndZ: int8 }


let packIndex x y z =
    (uint (byte x) <<< 16)
    ||| (uint (byte y) <<< 8)
    ||| uint (byte z)


let parse input =
    let cells =
        Seq.indexed input
        |> Seq.fold (fun map (y, line) ->
            Seq.indexed line
            |> Seq.fold (fun map (x, c) ->
                match c with
                | '#' -> Map.add (packIndex (int8 x) (int8 y) 0y) true map
                | '.' -> map
                | _ -> Monke.invalidInput c) map) Map.empty

    let lengthX =
        input |> Seq.item 0 |> Seq.length |> int8

    let lengthY = input |> Seq.length |> int8

    { Cells = cells
      EndX = lengthX - 1y
      EndY = lengthY - 1y
      EndZ = 0y
      StartX = 0y
      StartY = 0y
      StartZ = 0y }


let seq3D (s1, e1) (s2, e2) (s3, e3) =
    Seq.collect (fun c1 ->
        seq { s2 .. e2 }
        |> Seq.map (fun c2 -> seq { s3 .. e3 } |> Seq.map (fun c3 -> c1, c2, c3))) (seq { s1 .. e1 })
    |> Seq.concat


let around =
    seq3D (-1y, 1y) (-1y, 1y) (-1y, 1y)
    |> Seq.filter ((<>) (0y, 0y, 0y))


let step (inp: Cube) =
    let currCells = inp.Cells

    let mutable minX = 100y
    let mutable maxX = -100y
    let mutable minY = 100y
    let mutable maxY = -100y
    let mutable minZ = 100y
    let mutable maxZ = -100y

    let cells =
        seq3D (inp.StartX - 1y, inp.EndX + 1y) (inp.StartY - 1y, inp.EndY + 1y) (inp.StartZ - 1y, inp.EndZ + 1y)
        |> Seq.fold (fun nextCells (x, y, z) ->
            let isActive =
                Map.tryFind (packIndex (int8 x) (int8 y) (int8 z)) currCells
                |> Option.defaultValue false

            let activeAround =
                around
                |> Seq.filter (fun (dX, dY, dZ) ->
                    Map.tryFind (packIndex (int8 x + dX) (int8 y + dY) (int8 z + dZ)) currCells
                    |> Option.defaultValue false)
                |> Seq.length

            let willBeActive =
                (isActive && (activeAround = 2 || activeAround = 3))
                || (not isActive && activeAround = 3)

            if willBeActive then
                if x < minX then minX <- x
                elif x > maxX then maxX <- x

                if y < minY then minY <- y
                elif y > maxY then maxY <- y

                if z < minZ then minZ <- z
                elif z > maxZ then maxZ <- z

                Map.add (packIndex x y z) true nextCells
            else
                nextCells) Map.empty

    { Cells = cells
      StartX = minX
      EndX = maxX
      StartY = minY
      EndY = maxY
      StartZ = minZ
      EndZ = maxZ }


let print (inp: Cube) =
    seq { inp.StartZ .. inp.EndZ }
    |> Seq.iter (fun z ->
        seq { inp.StartY .. inp.EndY }
        |> Seq.iter (fun y ->
            seq { inp.StartX .. inp.EndX }
            |> Seq.iter (fun x ->
                if Map.tryFind (packIndex x y z) inp.Cells
                   |> Option.defaultValue false then
                    printf "%c" '#'
                else
                    printf "%c" '.')
            printfn "")
        printfn "")


let step6 =
    step >> step >> step >> step >> step >> step


let len { Cells = cells } = cells |> Map.toSeq |> Seq.length


".#.
..#
###"
|> Monke.String.splitLines
|> Seq.map Monke.String.chars
|> parse
|> step6
|> len
|> printfn "Example 1: %d"


input
|> parse
|> step6
|> len
|> printfn "Part One: %d"


type Cube4D =
    { Cells: Map<uint, bool>
      StartX: int8
      EndX: int8
      StartY: int8
      EndY: int8
      StartZ: int8
      EndZ: int8
      StartW: int8
      EndW: int8 }


let packIndex4D x y z w =
    (uint (byte x) <<< 24)
    ||| (uint (byte y) <<< 16)
    ||| (uint (byte z) <<< 8)
    ||| uint (byte w)


let seq4D (s1, e1) (s2, e2) (s3, e3) (s4, e4) =
    Seq.collect (fun c1 ->
        seq { s2 .. e2 }
        |> Seq.map (fun c2 ->
            seq { s3 .. e3 }
            |> Seq.map (fun c3 ->
                (seq { s4 .. e4 }
                 |> Seq.map (fun c4 -> c1, c2, c3, c4))))) (seq { s1 .. e1 })
    |> Seq.concat
    |> Seq.concat


let around4D =
    seq4D (-1y, 1y) (-1y, 1y) (-1y, 1y) (-1y, 1y)
    |> Seq.filter ((<>) (0y, 0y, 0y, 0y))


let idx3DTo4D i w = (i <<< 8) ||| uint (byte w)


let to4D (inp: Cube) =
    { Cells =
          inp.Cells
          |> Map.toSeq
          |> Seq.map (fun (i, v) -> idx3DTo4D i 0y, v)
          |> Map.ofSeq
      StartX = inp.StartX
      EndX = inp.EndX
      StartY = inp.StartY
      EndY = inp.EndY
      StartZ = inp.StartZ
      EndZ = inp.EndZ
      StartW = 0y
      EndW = 0y }


let step4D (inp: Cube4D) =
    let currCells = inp.Cells

    let mutable minX = 100y
    let mutable maxX = -100y
    let mutable minY = 100y
    let mutable maxY = -100y
    let mutable minZ = 100y
    let mutable maxZ = -100y
    let mutable minW = 100y
    let mutable maxW = -100y

    let cells =
        seq4D
            (inp.StartX - 1y, inp.EndX + 1y)
            (inp.StartY - 1y, inp.EndY + 1y)
            (inp.StartZ - 1y, inp.EndZ + 1y)
            (inp.StartW - 1y, inp.EndW + 1y)
        |> Seq.fold (fun nextCells (x, y, z, w) ->
            let isActive =
                Map.tryFind (packIndex4D (int8 x) (int8 y) (int8 z) (int8 w)) currCells
                |> Option.defaultValue false

            let activeAround =
                around4D
                |> Seq.filter (fun (dX, dY, dZ, dW) ->
                    Map.tryFind (packIndex4D (x + dX) (y + dY) (z + dZ) (w + dW)) currCells
                    |> Option.defaultValue false)
                |> Seq.length

            let willBeActive =
                (isActive && (activeAround = 2 || activeAround = 3))
                || (not isActive && activeAround = 3)

            if willBeActive then
                if x < minX then minX <- x
                elif x > maxX then maxX <- x

                if y < minY then minY <- y
                elif y > maxY then maxY <- y

                if z < minZ then minZ <- z
                elif z > maxZ then maxZ <- z

                if w < minW then minW <- w
                elif w > maxW then maxW <- w

                Map.add (packIndex4D x y z w) true nextCells
            else
                nextCells

            ) Map.empty

    { Cells = cells
      StartX = minX
      EndX = maxX
      StartY = minY
      EndY = maxY
      StartZ = minZ
      EndZ = maxZ
      StartW = minW
      EndW = maxW }


let step4D6 =
    step4D
    >> step4D
    >> step4D
    >> step4D
    >> step4D
    >> step4D


let len4D ({ Cells = cells }: Cube4D) = cells |> Map.toSeq |> Seq.length


".#.
..#
###"
|> Monke.String.splitLines
|> Seq.map Monke.String.chars
|> parse
|> to4D
|> step4D6
|> len4D
|> printfn "Example 2: %d"


input
|> parse
|> to4D
|> step4D6
|> len4D
|> printfn "Part Two: %d"
