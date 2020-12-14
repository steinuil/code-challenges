#load "monke.fs"



let parse inp =
    let departureTime = Seq.item 0 inp

    let schedules =
        Seq.item 1 inp |> Monke.String.split [ "," ]

    departureTime, schedules


let input =
    Monke.IO.readLines "day-13.input" |> parse

let example =
    "939
7,13,x,x,59,x,31,19"
    |> Monke.String.splitLines
    |> parse


let parse1 (departureTime, schedules) =
    int departureTime, schedules |> Seq.filter ((<>) "x") |> Seq.map int


let findEarliestTime ((departureTime: int), schedules) =
    schedules
    |> Seq.map (fun id ->
        let rem = departureTime % id
        departureTime + id - rem, id)
    |> Seq.minBy fst
    |> fun (busDepartureTime, id) -> (busDepartureTime - departureTime) * id


example
|> parse1
|> findEarliestTime
|> printfn "Example 1: %d"


input
|> parse1
|> findEarliestTime
|> printfn "Part One: %d"


let parse2 schedules =
    schedules
    |> Seq.indexed
    |> Seq.filter (fun (_, id) -> id <> "x")
    |> Seq.map (fun (i, id) -> (bigint i, bigint (int id)))


let findSequence schedules =
    Seq.initInfinite id
    |> Seq.pick (fun n ->
        let departure = bigint n * (Seq.item 0 schedules |> snd)

        let check (idx, id) = id - (departure % id) = idx

        if Seq.forall check (Seq.tail schedules) then Some departure else None)


let modInv a n =
    System.Numerics.BigInteger.ModPow(a, n - bigint 2, n)

let chineseRemainderGauss schedules =
    let prod =
        schedules |> Seq.map snd |> Seq.reduce (*)

    let res =
        Seq.sumBy (fun (i, id) ->
            let p = prod / id
            let m = modInv p id
            (id - (i % id)) * p * m) schedules

    res % prod


example
|> snd
|> parse2
|> findSequence
|> printfn "Example 2: %A"

example
|> snd
|> parse2
|> chineseRemainderGauss
|> printfn "Example 2: %A"

input
|> snd
|> parse2
|> chineseRemainderGauss
|> printfn "Part Two: %A"
