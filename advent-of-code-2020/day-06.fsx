#load "monke.fs"


let example =
    Monke.String.split [ "\r\n\r\n"; "\n\n" ] "abc

a
b
c

ab
ac

a
a
a
a

b"


let input =
    Monke.IO.readToString "day-06.input"
    |> Monke.String.split [ "\r\n\r\n"
                            "\n\n" ]


let countDistinctQuestions (group: string) =
    group.ToCharArray()
    |> Seq.filter (fun c -> c <> '\r' && c <> '\n')
    |> Seq.distinct
    |> Seq.length


example
|> Seq.sumBy countDistinctQuestions
|> printfn "Example 1: %d"


input
|> Seq.sumBy countDistinctQuestions
|> printfn "Part One: %d"


let countQuestionsThatEverybodyAnswered group =
    group
    |> Seq.concat
    |> Seq.distinct
    |> Seq.map (fun question -> group |> Seq.forall (Seq.contains question))
    |> Seq.filter id
    |> Seq.length


let splitGroups =
    Monke.String.split [ "\r\n"; "\n" ]
    >> Seq.map (fun person -> person.ToCharArray())


example
|> Seq.sumBy (splitGroups >> countQuestionsThatEverybodyAnswered)
|> printfn "Example 2: %d"


input
|> Seq.sumBy (splitGroups >> countQuestionsThatEverybodyAnswered)
|> printfn "Part Two: %d"
