#load "monke.fs"


let input = Monke.IO.readLines "day-03.input"


let step y input =
    let x = (y * 3) % String.length (Seq.head input)
    match Seq.tryItem y input with
    | Some line -> Some(line.[x] = '#')
    | None -> None


let rec loop y count input =
    match step y input with
    | Some true -> loop (y + 1) (count + 1) input
    | Some false -> loop (y + 1) count input
    | None -> count


let example =
    "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#".Split([| "\r\n"; "\n" |], System.StringSplitOptions.None)


example |> loop 0 0 |> printfn "Example 1: %d"


input |> loop 0 0 |> printfn "Part One: %d"


let step2 y ix iy input =
    let x =
        int ((int64 y * int64 ix) % int64 (String.length (Seq.head input)))

    let y = (y * iy)
    match Seq.tryItem y input with
    | Some line -> Some(line.[x] = '#')
    | None -> None


let loop2 ix iy input =
    let rec loop2 y count =
        match step2 y ix iy input with
        | Some true -> loop2 (y + 1) (count + 1L)
        | Some false -> loop2 (y + 1) count
        | None -> count

    loop2 0 0L


let collectResults input =
    [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ]
    |> Seq.map (fun (ix, iy) -> loop2 ix iy input)
    |> Seq.reduce (*)


example
|> collectResults
|> printfn "Example 2: %d"


input |> collectResults |> printfn "Part Two: %d"
