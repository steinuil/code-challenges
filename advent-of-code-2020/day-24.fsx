#load "monke.fs"


let input = "day-24.input" |> Monke.IO.readLines


let example = "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"


type Direction =
    | East
    | SouthEast
    | SouthWest
    | West
    | NorthWest
    | NorthEast


let parse inp =
    inp
    |> Seq.map
        (fun line ->
            let rec loop rest =
                seq {
                    match rest with
                    | [] -> ()
                    | 'e' :: rest ->
                        yield East
                        yield! loop rest
                    | 's' :: 'e' :: rest ->
                        yield SouthEast
                        yield! loop rest
                    | 's' :: 'w' :: rest ->
                        yield SouthWest
                        yield! loop rest
                    | 'w' :: rest ->
                        yield West
                        yield! loop rest
                    | 'n' :: 'w' :: rest ->
                        yield NorthWest
                        yield! loop rest
                    | 'n' :: 'e' :: rest ->
                        yield NorthEast
                        yield! loop rest
                    | c :: _ -> Monke.invalidInput c
                }

            Monke.String.chars line |> Seq.toList |> loop)


let follow path =
    path
    |> Seq.fold
        (fun (x, y) ->
            function
            | East -> (x + 2, y)
            | West -> (x - 2, y)
            | SouthEast -> (x + 1, y + 1)
            | SouthWest -> (x - 1, y + 1)
            | NorthEast -> (x + 1, y - 1)
            | NorthWest -> (x - 1, y - 1))
        (0, 0)


let countBlack inp =
    inp
    |> Seq.map follow
    |> Seq.groupBy id
    |> Seq.filter (fun (_, ls) -> Seq.length ls % 2 <> 0)
    |> Seq.length


example
|> Monke.String.splitLines
|> parse
|> countBlack
|> printfn "Example 1: %d"


input
|> parse
|> countBlack
|> printfn "Part One: %d"


let pack x y =
    (uint64 (uint32 x) <<< 16) ||| uint64 (uint32 y)


let countAdjacentBlack (x, y) black =
    [ 2, 0
      1, 1
      -1, 1
      -2, 0
      -1, -1
      1, -1 ]
    |> Seq.filter (fun (dx, dy) -> Set.contains ((dx + x), (dy + y)) black)
    |> Seq.length


type Grid =
    { Black: Set<int * int>
      MinX: int
      MaxX: int
      MinY: int
      MaxY: int }


let step (inp: Grid) =
    let black = inp.Black

    let mutable minX = 100
    let mutable maxX = -100
    let mutable minY = 100
    let mutable maxY = -100

    let nextBlack =
        Monke.seq2D (inp.MinX - 2, inp.MaxX + 2) (inp.MinY - 1, inp.MaxY + 1)
        |> Seq.filter (fun (x, y) -> if y % 2 = 0 then x % 2 = 0 else x % 2 <> 0)
        |> Seq.fold
            (fun nextBlack (x, y) ->
                let adj = countAdjacentBlack (x, y) black

                let isBlack = Set.contains (x, y) black

                let willBeBlack =
                    if isBlack then not (adj = 0 || adj > 2) else adj = 2

                if willBeBlack then
                    if x < minX then minX <- x
                    elif x > maxX then maxX <- x

                    if y < minY then minY <- y
                    elif y > maxY then maxY <- y

                    Set.add (x, y) nextBlack
                else
                    nextBlack)
            Set.empty

    { Black = nextBlack
      MinX = minX
      MaxX = maxX
      MinY = minY
      MaxY = maxY }


let play n inp =
    let black =
        inp
        |> Seq.map follow
        |> Seq.groupBy id
        |> Seq.filter (fun (_, ls) -> Seq.length ls % 2 <> 0)
        |> Seq.map (fun (coords, _) -> coords)
        |> Set.ofSeq

    let rec loop n grid =
        if n = 0 then grid.Black else loop (n - 1) (step grid)

    loop
        n
        { Black = black
          MinX = -200
          MinY = -200
          MaxX = 200
          MaxY = 200 }
    |> Set.count


example
|> Monke.String.splitLines
|> parse
|> play 100
|> printfn "Example 2: %d"


input
|> parse
|> play 100
|> printfn "Part Two: %d"
