#load "monke.fs"

open System


let inputFile = "day-20.test"


let tileWidth = 10


type Direction =
    | Top
    | Right
    | Bottom
    | Left


module Edge =
    let fromBits: seq<bool> -> uint16 =
        Seq.fold (fun edge bit -> (edge <<< 1) ||| if bit then 1us else 0us) 0us

    let reverse edge =
        seq { 0 .. tileWidth - 1 }
        |> Seq.fold
            (fun rev i ->
                rev
                ||| (((edge &&& (1us <<< i)) >>> i)
                     <<< tileWidth - 1 - i))
            0us

// let invert edge =
//     (~~~edge) &&& ((1us <<< tileWidth) - 1us)


type Rotation =
    | R0
    | R90
    | R180
    | R270


type Tile =
    { num: int
      bits: Collections.BitArray
      rotation: Rotation
      flipped: bool }

module Tile =
    let private w = tileWidth - 1

    let parse num (str: string) =
        let bits =
            Collections.BitArray(tileWidth * tileWidth)

        str.Split("\n")
        |> Seq.collect (fun line -> line.ToCharArray())
        |> Seq.indexed
        |> Seq.iter (fun (i, char) -> if char = '#' then bits.[i] <- true)

        { num = num
          bits = bits
          rotation = R0
          flipped = false }


    let projectCoordinates x y tile =
        match tile.rotation, tile.flipped with
        | R0, false -> (x, y)
        | R0, true -> (w - x, y)

        | R180, false -> (w - x, w - y)
        | R180, true -> (x, w - y)

        // A tile is first rotated and then flipped horizontally.
        | R90, false -> (w - y, x)
        | R90, true -> (y, x)

        | R270, false -> (y, w - x)
        | R270, true -> (w - y, w - x)


    let index x y tile =
        let x, y = projectCoordinates x y tile
        tile.bits.[y * tileWidth + x]


    let fprint out tile =
        for y in 0 .. w do
            for x in 0 .. w do
                if index x y tile then
                    fprintf out "#"
                else
                    fprintf out "."

            fprintfn out ""


    let edge e tile =
        match e with
        | Top -> seq { 0 .. w } |> Seq.map (fun x -> (x, 0))
        | Right -> seq { 0 .. w } |> Seq.map (fun y -> (w, y))
        | Bottom -> seq { w .. -1 .. 0 } |> Seq.map (fun x -> (x, w))
        | Left -> seq { w .. -1 .. 0 } |> Seq.map (fun y -> (0, y))
        |> Seq.map (fun (x, y) -> index x y tile)
        |> Edge.fromBits


    let edges tile =
        [ Top; Right; Bottom; Left ]
        |> Seq.map (fun dir -> edge dir tile)


let input =
    (Monke.IO.readToString inputFile).Split("\n\n")
    |> Seq.map (fun line ->
        let tileNum, tile =
            match line.Split("\n", 2) with
            | [| tileNum; tile |] -> tileNum, tile
            | _ -> failwith "invalid input"

        let tileNum =
            tileNum.Substring(5, tileNum.Length - 6)
            |> Int32.Parse

        Tile.parse tileNum tile)


let inputById =
    input
    |> Seq.fold (fun map tile -> Map.add tile.num tile map) Map.empty


let tileIdsByEdge =
    input
    |> Seq.fold
        (fun map tile ->
            [ Top; Right; Bottom; Left ]
            |> Seq.fold
                (fun map direction ->
                    let edge = Tile.edge direction tile
                    let revEdge = Edge.reverse edge

                    let map =
                        match Map.tryFind edge map with
                        | Some tileIds -> Map.add edge (tile.num :: tileIds) map
                        | None -> Map.add edge [ tile.num ] map

                    match Map.tryFind revEdge map with
                    | Some tileIds -> Map.add revEdge (tile.num :: tileIds) map
                    | None -> Map.add revEdge [ tile.num ] map)
                map)
        Map.empty


// Part 1
do
    input
    |> Seq.choose (fun tile ->
        let lonelyEdges =
            tile
            |> Tile.edges
            |> Seq.filter (fun edge ->
                let t1 = Map.find edge tileIdsByEdge

                let t2 =
                    Map.find (Edge.reverse edge) tileIdsByEdge

                List.length t1 = 1
                && List.length t2 = 1
                && t1 = t2)
            |> Seq.length

        if lonelyEdges = 2 then
            Some(int64 tile.num)
        else
            None)
    |> Seq.reduce (*)
    |> printfn "Part One: %d"
