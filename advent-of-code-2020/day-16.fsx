#load "monke.fs"


let input = "day-16.input" |> Monke.IO.readLines


type Range<'a> = { From: 'a; To: 'a }


type Field =
    { Name: string
      Range1: Range<int>
      Range2: Range<int> }


let parseField =
    function
    | Monke.RegexMatch @"^(.+): (\d+)-(\d+) or (\d+)-(\d+)$" [ fieldName; from1; to1; from2; to2 ] ->
        Some
            { Name = fieldName
              Range1 = { From = int from1; To = int to1 }
              Range2 = { From = int from2; To = int to2 } }
    | _ -> None


let parseTicket =
    Monke.String.split [ "," ] >> Seq.map int


let parse inp =
    let rec takeFields fields inp =
        match parseField <| Seq.head inp with
        | Some field -> takeFields (field :: fields) (Seq.tail inp)
        | None -> List.rev fields, inp

    let takeNearbyTickets = Seq.map parseTicket

    let fields, rest = takeFields [] inp
    let rest = Seq.skip 2 rest
    let yourTicket = parseTicket (Seq.head rest)
    let rest = Seq.skip 3 rest
    let nearbyTickets = takeNearbyTickets rest
    fields, yourTicket, nearbyTickets


let isValidField { Range1 = r1; Range2 = r2 } n =
    Monke.isBetween r1.From r1.To n
    || Monke.isBetween r2.From r2.To n


let isValid fields n =
    fields
    |> Seq.exists (fun field -> isValidField field n)


let countInvalidNearbyTickets (fields, _, nearbyTickets) =
    nearbyTickets
    |> Seq.sumBy (Seq.filter (not << isValid fields) >> Seq.sum)


"class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"
|> Monke.String.splitLines
|> parse
|> countInvalidNearbyTickets
|> printfn "Example 1: %d"


input
|> parse
|> countInvalidNearbyTickets
|> printfn "Part One: %d"


let isTicketValid fields = Seq.forall (isValid fields)


let filterValidTickets (fields, yourTicket, nearbyTickets) =
    let nearbyTickets =
        nearbyTickets |> Seq.filter (isTicketValid fields)

    fields, yourTicket, nearbyTickets


let findFieldsOrder fields inp =
    Monke.Seq.permutations fields
    |> Seq.find
        (fun fields ->
            printfn
                "trying with order %s"
                (fields
                 |> Seq.map (fun { Name = name } -> name)
                 |> String.concat ", ")

            inp
            |> Seq.forall
                (fun ticket ->
                    Seq.zip fields ticket
                    |> Seq.forall (fun (field, num) -> isValidField field num)))


let part2 (fields, yourTicket, nearbyTickets) =
    let fields =
        findFieldsOrder fields (Seq.append (Seq.singleton yourTicket) nearbyTickets)

    Seq.zip fields yourTicket
    |> Seq.map (fun ({ Name = name }, n) -> (name, n))
    |> Seq.filter (fun (name, _) -> name.StartsWith("departure"))
    |> Seq.sumBy snd


let listExtract f l =
    let rec loop acc =
        function
        | [] -> failwith "a"
        | x :: xs when f x -> x, (List.rev acc) @ xs
        | x :: xs -> loop (x :: acc) xs

    loop [] l


let validFieldsPerColumn (fields, yourTicket, nearbyTickets) =
    let ticketsByField =
        (Seq.append (Seq.singleton yourTicket) nearbyTickets)
        |> Seq.transpose

    Seq.fold
        (fun fieldsPerColumn column ->
            Seq.filter (fun field -> Seq.forall (fun n -> isValidField field n) column) fields
            :: fieldsPerColumn)
        []
        ticketsByField
    |> List.rev


let solveWithMsPaint (fields, yourTicket, nearbyTickets) =
    let fs =
        validFieldsPerColumn (fields, yourTicket, nearbyTickets)

    let toNum =
        fields
        |> Seq.indexed
        |> Seq.map (fun (v, k) -> (k.Name, v))
        |> Map.ofSeq

    let nums =
        fs
        |> Seq.map (Seq.map (fun { Name = name } -> Map.find name toNum))


    Seq.iter
        (printfn "%s"
         << String.concat ", "
         << Seq.map string)
        nums

    toNum
    |> Map.toSeq
    |> Seq.iter (fun (name, num) -> printfn "%s = %d" name num)

    (* Solved by hand, see day-16.png *)

    let inputOrder =
        [ "row"
          "seat"
          "arrival location"
          "duration"
          "departure date"
          "route"
          "wagon"
          "departure station"
          "price"
          "departure location"
          "departure platform"
          "departure track"
          "zone"
          "type"
          "departure time"
          "arrival track"
          "arrival station"
          "class"
          "arrival platform"
          "train" ]

    Seq.zip inputOrder yourTicket
    |> Seq.filter (fun (name, _) -> name.StartsWith("departure"))
    |> Seq.map (snd >> int64)
    |> Seq.reduce (*)
    |> printfn "Part Two: %d"


let solveProperly (fields, yourTicket, nearbyTickets) =
    let fs =
        validFieldsPerColumn (fields, yourTicket, nearbyTickets)
        |> Seq.indexed

    let rec loop solved rest =
        if Seq.isEmpty rest then
            solved |> List.sortBy fst |> List.map snd
        else
            let (i, onePossibilities), rest =
                rest
                |> listExtract (fun (_, possibilities) -> Seq.length possibilities = 1)

            let field = onePossibilities |> Seq.item 0

            let rest =
                List.map (fun (i, possibilities) -> i, Seq.filter (fun name -> name <> field) possibilities) rest

            loop ((i, field) :: solved) rest

    let inputOrder =
        loop [] (Seq.toList fs)
        |> Seq.map (fun { Name = name } -> name)

    Seq.zip inputOrder yourTicket
    |> Seq.filter (fun (name, _) -> name.StartsWith("departure"))
    |> Seq.map (snd >> int64)
    |> Seq.reduce (*)


input
|> parse
|> filterValidTickets
|> solveProperly
|> printfn "Part Two: %d"
