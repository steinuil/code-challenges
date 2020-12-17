#load "monke.fs"


let input =
    "day-17.input"
    |> Monke.IO.readLines
    |> Seq.map Monke.String.chars


type Cube =
    { Cells: Map<int * int * int, bool>
      StartX: int
      EndX: int
      StartY: int
      EndY: int
      StartZ: int
      EndZ: int }


let parse input =
    let cells =
        Seq.indexed input
        |> Seq.fold (fun map (y, line) ->
            Seq.indexed line
            |> Seq.fold (fun map (x, c) ->
                match c with
                | '#' -> Map.add (x, y, 0) true map
                | '.' -> map
                | _ -> Monke.invalidInput c) map) Map.empty

    let lengthX = input |> Seq.item 0 |> Seq.length
    let lengthY = input |> Seq.length

    { Cells = cells
      EndX = lengthX - 1
      EndY = lengthY - 1
      EndZ = 0
      StartX = 0
      StartY = 0
      StartZ = 0 }


let seq3D (s1, e1) (s2, e2) (s3, e3) =
    Seq.collect (fun c1 ->
        seq { s2 .. e2 }
        |> Seq.map (fun c2 -> seq { s3 .. e3 } |> Seq.map (fun c3 -> c1, c2, c3))) (seq { s1 .. e1 })
    |> Seq.concat


let around =
    seq3D (-1, 1) (-1, 1) (-1, 1)
    |> Seq.filter ((<>) (0, 0, 0))


let step (inp: Cube) =
    let currCells = inp.Cells

    let mutable minX = 100
    let mutable maxX = -100
    let mutable minY = 100
    let mutable maxY = -100
    let mutable minZ = 100
    let mutable maxZ = -100

    let cells =
        seq3D (inp.StartX - 1, inp.EndX + 1) (inp.StartY - 1, inp.EndY + 1) (inp.StartZ - 1, inp.EndZ + 1)
        |> Seq.fold (fun nextCells (x, y, z) ->

            let isActive =
                Map.tryFind (x, y, z) currCells
                |> Option.defaultValue false

            let activeAround =
                around
                |> Seq.filter (fun (dX, dY, dZ) ->
                    Map.tryFind (x + dX, y + dY, z + dZ) currCells
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

                Map.add (x, y, z) true nextCells
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
                if Map.tryFind (x, y, z) inp.Cells
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
    { Cells: Map<int * int * int * int, bool>
      StartX: int
      EndX: int
      StartY: int
      EndY: int
      StartZ: int
      EndZ: int
      StartW: int
      EndW: int }



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
    seq4D (-1, 1) (-1, 1) (-1, 1) (-1, 1)
    |> Seq.filter ((<>) (0, 0, 0, 0))


let to4D (inp: Cube) =
    { Cells =
          inp.Cells
          |> Map.toSeq
          |> Seq.map (fun ((x, y, z), v) -> (x, y, z, 0), v)
          |> Map.ofSeq
      StartX = inp.StartX
      EndX = inp.EndX
      StartY = inp.StartY
      EndY = inp.EndY
      StartZ = inp.StartZ
      EndZ = inp.EndZ
      StartW = 0
      EndW = 0 }


let step4D (inp: Cube4D) =
    let currCells = inp.Cells

    let mutable minX = 100
    let mutable maxX = -100
    let mutable minY = 100
    let mutable maxY = -100
    let mutable minZ = 100
    let mutable maxZ = -100
    let mutable minW = 100
    let mutable maxW = -100

    let cells =
        seq4D
            (inp.StartX - 1, inp.EndX + 1)
            (inp.StartY - 1, inp.EndY + 1)
            (inp.StartZ - 1, inp.EndZ + 1)
            (inp.StartW - 1, inp.EndW + 1)
        |> Seq.fold (fun nextCells (x, y, z, w) ->
            let isActive =
                Map.tryFind (x, y, z, w) currCells
                |> Option.defaultValue false

            let activeAround =
                around4D
                |> Seq.filter (fun (dX, dY, dZ, dW) ->
                    Map.tryFind (x + dX, y + dY, z + dZ, w + dW) currCells
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

                Map.add (x, y, z, w) true nextCells
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


#time

input
|> parse
|> to4D
|> step4D6
|> len4D
|> printfn "Part One: %d"

#time
