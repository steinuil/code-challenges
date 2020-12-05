#load "monke.fs"


let input =
    Monke.IO.readLines "day-05.input"
    |> Seq.map (fun x -> x.ToCharArray())


let bsearch max up down chars =
    let step min max char =
        let interval = (max - min) / 2
        if char = up then (min + interval, max)
        elif char = down then (min, max - interval)
        else failwith "unreachable"

    let rec loop min max =
        function
        | [] -> min
        | c :: rest ->
            let min, max = step min max c
            loop min max rest

    loop 0 max chars


let getSeat chars =
    let row =
        Seq.take 7 chars
        |> Seq.toList
        |> bsearch 128 'B' 'F'

    let column =
        Seq.skip 7 chars
        |> Seq.toList
        |> bsearch 8 'R' 'L'

    row, column, row * 8 + column


"FBFBBFFRLR".ToCharArray()
|> getSeat
|> printfn "Example 1: %A"

"BFFFBBFRRR".ToCharArray()
|> getSeat
|> printfn "Example 2: %A"

"FFFBBBFRRR".ToCharArray()
|> getSeat
|> printfn "Example 3: %A"

"BBFFBBFRLL".ToCharArray()
|> getSeat
|> printfn "Example 4: %A"


let third (_, _, x) = x


let seatIds = input |> Seq.map (getSeat >> third)

seatIds |> Seq.max |> printfn "Part One: %d"

seatIds
|> Seq.sort
|> Seq.windowed 2
|> Seq.choose (function
    | [| idMinus1; idPlus1 |] -> if idPlus1 - idMinus1 = 2 then Some(idMinus1 + 1) else None
    | _ -> None)
|> Seq.exactlyOne
|> printfn "Part Two: %d"
