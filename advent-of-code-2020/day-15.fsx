#load "monke.fs"


let parse =
    Monke.String.split [ "," ] >> Seq.map int


let input =
    "day-15.input" |> Monke.IO.readToString |> parse


let recitate til seed =
    let rec loop spoken last turn =
        if turn > til then
            last
        else
            let curr =
                Map.tryFind last spoken
                |> Option.map (fun t -> turn - 1 - t)
                |> Option.defaultValue 0

            let spoken = Map.add last (turn - 1) spoken

            loop spoken curr (turn + 1)

    let spoken =
        Seq.indexed seed
        |> Seq.map (fun (v, k) -> k, v + 1)
        |> Map.ofSeq

    loop spoken (Seq.last seed) (Seq.length seed + 1)


"0,3,6"
|> parse
|> recitate 2020
|> printfn "Example 1: %d"

"1,3,2"
|> parse
|> recitate 2020
|> printfn "Example 2: %d"

"2,1,3"
|> parse
|> recitate 2020
|> printfn "Example 3: %d"

"3,1,2"
|> parse
|> recitate 2020
|> printfn "Example 7: %d"

input |> recitate 2020 |> printfn "Part One: %d"

let recitate2 til seed =
    let spoken = Monke.Dict.empty ()

    let rec loop last turn =
        if turn > til then
            last
        else
            let curr =
                Monke.Dict.tryFind last spoken
                |> Option.map (fun t -> turn - 1 - t)
                |> Option.defaultValue 0

            Monke.Dict.add last (turn - 1) spoken

            loop curr (turn + 1)

    Seq.indexed seed
    |> Seq.iter (fun (v, k) -> Monke.Dict.add k (v + 1) spoken)

    loop (Seq.last seed) (Seq.length seed + 1)

"0,3,6"
|> parse
|> recitate2 30000000
|> printfn "Example 8: %d"

input
|> recitate2 30000000
|> printfn "Part Two: %d"
