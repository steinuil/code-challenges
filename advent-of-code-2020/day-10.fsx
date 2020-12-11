#load "monke.fs"


let input =
    "day-10.input"
    |> Monke.IO.readLines
    |> Seq.map int


let sortJoltages input =
    let max = input |> Seq.max
    Seq.concat [ Seq.sort input
                 Seq.singleton (max + 3) ]


let differences: seq<int> -> int * int =
    sortJoltages
    >> Seq.fold (fun (by1, by3, last) curr ->
        match curr - last with
        | 1 -> (by1 + 1, by3, curr)
        | 3 -> (by1, by3 + 1, curr)
        | n -> failwithf "invalid difference: %d" n) (0, 0, 0)
    >> fun (by1, by3, _) -> by1, by3


let example1 =
    "16
10
15
5
1
11
7
19
6
12
4"
    |> Monke.String.splitLines
    |> Seq.map int

example1
|> differences
|> fun (by1, by3) -> by1 * by3
|> printfn "Example 1: %d"


let example2 =
    "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"
    |> Monke.String.splitLines
    |> Seq.map int


example2
|> differences
|> fun (by1, by3) -> by1 * by3
|> printfn "Example 2: %d"


input
|> differences
|> fun (by1, by3) -> by1 * by3
|> printfn "Part One: %d"

#nowarn "25"

let contiguous input =
    Seq.concat [ seq [ 0 ]
                 sortJoltages input ]
    |> Seq.pairwise
    |> Seq.fold (fun (currSeq, seqs) (last, curr) ->
        if curr - last = 1 then (currSeq + 1, seqs)
        elif currSeq > 0 then (0, currSeq :: seqs)
        else (currSeq, seqs)) (0, [])
    |> snd
    |> List.rev


let toTribonacci =
    function
    | 1 -> 1L
    | 2 -> 2L
    | 3 -> 4L
    | 4 -> 7L
    | n -> failwithf "invalid: %d" n


example1
|> contiguous
|> Seq.map toTribonacci
|> Seq.reduce (*)
|> printfn "Example 3: %d"


example2
|> contiguous
|> Seq.map toTribonacci
|> Seq.reduce (*)
|> printfn "Example 4: %d"


input
|> contiguous
|> Seq.map toTribonacci
|> Seq.reduce (*)
|> printfn "Part Two: %d"
