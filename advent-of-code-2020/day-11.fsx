#load "monke.fs"


let input =
    "day-11.input"
    |> Monke.IO.readLines
    |> Seq.map seq
    |> array2D


let example =
    "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
    |> Monke.String.splitLines
    |> Seq.map seq
    |> array2D


let step (board: char [,]) =
    let get y x =
        try
            Array2D.get board y x
        with _ -> '.'


    let adjacent y x =
        Seq.concat [ seq { x - 1 .. x + 1 } |> Seq.map (get (y - 1))
                     seq [ x - 1; x + 1 ] |> Seq.map (get y)
                     seq { x - 1 .. x + 1 } |> Seq.map (get (y + 1)) ]
        |> Seq.filter ((<>) '.')

    let shouldBecomeOccupied = Seq.forall ((<>) '#')

    let shouldBecomeEmpty s =
        s |> Seq.filter ((=) '#') |> Seq.length >= 4

    let newBoard = Array2D.copy board
    let mutable hasChanged = false

    board
    |> Array2D.iteri (fun y x ->
        let adj = adjacent y x
        function
        | 'L' when shouldBecomeOccupied adj ->
            newBoard.[y, x] <- '#'
            hasChanged <- true
        | '#' when shouldBecomeEmpty adj ->
            newBoard.[y, x] <- 'L'
            hasChanged <- true
        | _ -> ())

    newBoard, hasChanged


let showArray2D (arr: char [,]) =
    seq {
        for y in 0 .. Array2D.length1 arr - 1 do
            for x in 0 .. Array2D.length2 arr - 1 do
                yield arr.[y, x]

            yield '\n'
    }


let untilFixpoint =
    let rec loop board =
        let newBoard, hasChanged = step board
        if hasChanged then loop newBoard else newBoard

    loop


let countOccupiedSeats (board: char [,]) =
    Monke.Array2D.toSeq board
    |> Seq.filter ((=) '#')
    |> Seq.length


example
|> untilFixpoint
|> countOccupiedSeats
|> printfn "Example 1: %d"


input
|> untilFixpoint
|> countOccupiedSeats
|> printfn "Part One: %d"


let step2 (board: char [,]) =
    let newBoard = Array2D.copy board
    let ly = Array2D.length1 board
    let lx = Array2D.length2 board

    let visibleOccupied y x =
        let rec check iy ix c =
            let currY = y + (iy * c)
            let currX = x + (ix * c)
            if currY < 0
               || currY >= ly
               || currX < 0
               || currX >= lx then
                false
            else
                match board.[currY, currX] with
                | '#' -> true
                | 'L' -> false
                | '.' -> check iy ix (c + 1)
                | _ -> failwith "a"

        seq [ check 0 1 1
              check 1 1 1
              check 1 0 1
              check 1 -1 1
              check 0 -1 1
              check -1 -1 1
              check -1 0 1
              check -1 1 1 ]
        |> Seq.filter id
        |> Seq.length

    let mutable hasChanged = false

    let shouldBecomeOccupied = (=) 0
    let shouldBecomeEmpty c = c >= 5

    board
    |> Array2D.iteri (fun y x ->
        let c = visibleOccupied y x
        function
        | 'L' when shouldBecomeOccupied c ->
            newBoard.[y, x] <- '#'
            hasChanged <- true
        | '#' when shouldBecomeEmpty c ->
            newBoard.[y, x] <- 'L'
            hasChanged <- true
        | _ -> ())

    newBoard, hasChanged


let untilFixpoint2 =
    let rec loop board =
        let newBoard, hasChanged = step2 board
        if hasChanged then loop newBoard else newBoard

    loop


example
|> untilFixpoint2
|> countOccupiedSeats
|> printfn "Example 2: %d"


input
|> untilFixpoint2
|> countOccupiedSeats
|> printfn "Part Two: %d"
