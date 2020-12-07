#load "monke.fs"


let parse =
    function
    | Monke.RegexMatch @"^(.+) bags contain no other bags\.$" [ bagColor ] -> bagColor, Seq.empty
    | Monke.RegexMatch @"^(.+) bags contain (.+)\.$" [ bagColor; contained ] ->
        let contained =
            contained
            |> Monke.String.split [ ", " ]
            |> Seq.map (fun bag ->
                match bag with
                | Monke.RegexMatch @"^(\d+) (.+) bags?$" [ count; color ] -> int count, color
                | _ -> failwith bag)

        bagColor, contained

    | inp -> failwith inp


let input =
    Monke.IO.readLines "day-07.input" |> Seq.map parse


let example =
    "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
    |> Monke.String.split [ "\r\n"; "\n" ]
    |> Seq.map parse


let count1 needle input =
    let cache = Monke.Dict.empty ()

    let rec check (bag, contained) =
        match Monke.Dict.tryFind bag cache with
        | Some b -> b
        | None ->
            let exists =
                Seq.exists (fun (_, bag) ->
                    bag = needle
                    || check (Seq.find (fun (b, _) -> b = bag) input)) contained

            Monke.Dict.add bag exists cache

            exists

    input |> Seq.filter check |> Seq.length


example
|> count1 "shiny gold"
|> printfn "Example 1: %d"


input
|> count1 "shiny gold"
|> printfn "Part One: %d"


let count2 needle (input: seq<string * seq<int * string>>) =
    let cache = Monke.Dict.empty ()

    let rec check bag =
        match Monke.Dict.tryFind bag cache with
        | Some n -> n
        | None ->
            let _, contained = Seq.find (fun (b, _) -> b = bag) input

            let count =
                Seq.sumBy (fun (n, b) -> n + n * check b) contained

            Monke.Dict.add bag count cache
            count

    check needle


"shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."
|> Monke.String.split [ "\r\n"; "\n" ]
|> Seq.map parse
|> count2 "shiny gold"
|> printfn "Example 2: %d"


input
|> count2 "shiny gold"
|> printfn "Part Two: %d"
