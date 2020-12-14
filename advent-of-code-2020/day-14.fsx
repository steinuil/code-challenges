#load "monke.fs"


let input = "day-14.input" |> Monke.IO.readLines


module Part1 =
    type Mask = { And: int64; Or: int64 }


    type SetMem = { At: int; Val: int64 }


    type Cmd =
        | Mask of Mask
        | SetMem of SetMem


    let initialMask = { And = 68719476735L; Or = 0L }


    let parse =
        function
        | Monke.RegexMatch @"^mask = (.+)$" [ mask ] ->
            mask
            |> Seq.rev
            |> Seq.indexed
            |> Seq.fold (fun mask ->
                function
                | (_, 'X') -> mask
                | (i, '0') ->
                    { mask with
                          And = mask.And &&& ~~~(1L <<< i) }
                | (i, '1') ->
                    { mask with
                          Or = mask.Or ||| (1L <<< i) }
                | _ -> failwith "unreachable") initialMask
            |> Mask

        | Monke.RegexMatch @"^mem\[(.+)\] = (.+)$" [ at; v ] -> SetMem { At = int at; Val = int64 v }

        | line -> failwith line


    let example =
        "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"
        |> Monke.String.splitLines
        |> Seq.map parse


    let execute instructions =
        let memory, _ =
            Seq.fold (fun (memory, ({ And = andMask; Or = orMask } as mask)) ->
                function
                | Mask mask -> memory, mask
                | SetMem { At = at; Val = v } -> (Map.add at ((v &&& andMask) ||| orMask) memory, mask))
                (Map.empty, initialMask) instructions

        Map.toSeq memory |> Seq.sumBy snd


    example |> execute |> printfn "Example 1: %d"

    input
    |> Seq.map parse
    |> execute
    |> printfn "Part One: %d"


module Part2 =
    type Mask = { And: int64; Or: int64 }


    type SetMem = { At: int64; Val: int64 }


    type Cmd =
        | Masks of seq<Mask>
        | SetMem of SetMem


    let floating =
        let rec loop andMask orMask rest =
            seq {
                match rest with
                | [] -> yield { And = andMask; Or = orMask }
                | (i, '0') :: rest -> yield! loop andMask orMask rest
                | (i, '1') :: rest -> yield! loop andMask (orMask ||| (1L <<< i)) rest
                | (i, 'X') :: rest ->
                    yield! loop (andMask &&& ~~~(1L <<< i)) orMask rest
                    yield! loop andMask (orMask ||| (1L <<< i)) rest
                | (_, c) :: _ -> Monke.invalidInput c
            }

        loop 68719476735L 0L


    let parse =
        function
        | Monke.RegexMatch @"^mask = (.+)$" [ mask ] ->
            let masks =
                mask
                |> Seq.rev
                |> Seq.indexed
                |> Seq.toList
                |> floating

            Masks masks

        | Monke.RegexMatch @"^mem\[(.+)\] = (.+)$" [ at; v ] -> SetMem { At = int64 at; Val = int64 v }

        | line -> Monke.invalidInput line


    let execute instructions =
        let memory, _ =
            Seq.fold (fun (memory, masks) ->
                function
                | Masks masks -> memory, masks
                | SetMem { At = at; Val = v } ->
                    let memory =
                        masks
                        |> Seq.fold (fun memory { And = andMask; Or = orMask } ->
                            Map.add ((at &&& andMask) ||| orMask) v memory) memory

                    (memory, masks)) (Map.empty, Seq.empty) instructions

        Map.toSeq memory |> Seq.sumBy snd


    "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"
    |> Monke.String.splitLines
    |> Seq.map parse
    |> execute
    |> printfn "Example 2: %d"


    input
    |> Seq.map parse
    |> execute
    |> printfn "Part Two: %d"
