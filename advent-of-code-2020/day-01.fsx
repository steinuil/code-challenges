#load "monke.fs"

let input =
    Monke.IO.readLines "day-01.input" |> Seq.map int

Seq.tryPick (fun x -> Seq.tryPick (fun y -> if x <> y && x + y = 2020 then Some(x * y) else None) input) input
|> Option.get
|> printfn "Part One: %d"

Seq.tryPick (fun x ->
    Seq.tryPick (fun y ->
        Seq.tryPick (fun z -> if x <> y && y <> z && x + y + z = 2020 then Some(x * y * z) else None) input) input)
    input
|> Option.get
|> printfn "Part Two: %d"
