#load "monke.fs"

type Password =
    { Pos1: int
      Pos2: int
      Letter: char
      Password: string }

let parseLine (line: string) =

    let colonIdx = line.IndexOf(':')
    let letter = line.[colonIdx - 1]

    let range = line.[0..colonIdx - 3]
    let dashIdx = range.IndexOf('-')
    let pos1 = range.[0..dashIdx - 1] |> int
    let pos2 = range.[dashIdx + 1..] |> int

    let password = line.[colonIdx + 2..]

    { Pos1 = pos1
      Pos2 = pos2
      Letter = letter
      Password = password }

let input =
    Monke.IO.readLines "day-02.input"
    |> Seq.map parseLine

let isValidPart1 { Pos1 = min; Pos2 = max; Letter = letter; Password = password } =
    let count =
        password
        |> Seq.filter (fun ch -> ch = letter)
        |> Seq.length

    count >= min && count <= max

input
|> Seq.filter isValidPart1
|> Seq.length
|> printfn "Part One: %d"

let isValidPart2 ({ Pos1 = pos1; Pos2 = pos2; Letter = letter; Password = password } as pw) =
    try
        (password.[pos1 - 1] = letter)
        <> (password.[pos2 - 1] = letter)
    with exn ->
        printfn "%A" pw
        raise exn

input
|> Seq.filter isValidPart2
|> Seq.length
|> printfn "Part Two: %d"
