#load "monke.fs"


let input =
    "day-09.input"
    |> Monke.IO.readLines
    |> Seq.map int64


let example =
    "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"
    |> Monke.String.splitLines
    |> Seq.map int64


let isValid preambleLength inp i =
    let preamble =
        inp
        |> Seq.skip (i - preambleLength)
        |> Seq.take preambleLength

    let n = Seq.item i inp

    preamble
    |> Seq.indexed
    |> Seq.exists (fun (ia, a) ->
        preamble
        |> Seq.indexed
        |> Seq.exists (fun (ib, b) -> ia <> ib && a + b = n))


let findFirstNotValid preambleLength inp =
    seq { preambleLength .. Seq.length inp - 1 }
    |> Seq.pick (fun i -> if not <| isValid preambleLength inp i then Some(Seq.item i inp) else None)


let invalidExample = example |> findFirstNotValid 5

invalidExample |> printfn "Example 1: %d"


let invalid = input |> findFirstNotValid 25

invalid |> printfn "Part One: %d"


let findContiguousSet inp result =
    let inpLen = Seq.length inp

    let rec find start len =
        if start >= inpLen || start + len >= inpLen then
            None
        else
            let ls = inp |> Seq.skip start |> Seq.take len
            let sum = ls |> Seq.sum
            if sum = result then
                let smallest = Seq.min ls
                let largest = Seq.max ls
                Some((smallest + largest))
            elif sum > result then
                None
            else
                find start (len + 1)

    seq { 0 .. inpLen - 1 }
    |> Seq.pick (fun i -> find i 1)


findContiguousSet example invalidExample
|> printfn "Example 2: %d"

findContiguousSet input invalid
|> printfn "Part Two: %d"
