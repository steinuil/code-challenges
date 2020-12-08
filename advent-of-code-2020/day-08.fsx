#load "monke.fs"


type Op =
    | Acc
    | Jmp
    | Nop


let parse inp =
    let operation, argument = inp |> Monke.String.splitInTwo " "

    let operation =
        match operation with
        | "nop" -> Nop
        | "acc" -> Acc
        | "jmp" -> Jmp
        | _ -> failwith operation

    let argument = int argument
    operation, argument


let detectLoop instructions =
    let rec loop visited pos acc =
        if List.contains pos visited then
            acc
        else
            match Seq.item pos instructions with
            | Jmp, n -> loop (pos :: visited) (pos + n) acc
            | Acc, n -> loop (pos :: visited) (pos + 1) (acc + n)
            | Nop, _ -> loop (pos :: visited) (pos + 1) acc

    loop [] 0 0


let example =
    "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
    |> Monke.String.splitLines
    |> Seq.map parse


let input =
    Monke.IO.readLines "day-08.input" |> Seq.map parse


example |> detectLoop |> printfn "Example 1: %d"


input |> detectLoop |> printfn "Part One: %d"


let tryExecute instructions =
    let count = Seq.length instructions

    let rec loop visited pos acc =
        if pos = count then
            Some acc
        elif pos >= count then
            None
        elif List.contains pos visited then
            None
        else
            match Seq.item pos instructions with
            | Jmp, n -> loop (pos :: visited) (pos + n) acc
            | Acc, n -> loop (pos :: visited) (pos + 1) (acc + n)
            | Nop, _ -> loop (pos :: visited) (pos + 1) acc

    loop [] 0 0


let tryFix instructions =
    instructions
    |> Seq.indexed
    |> Seq.filter (function
        | (_, (Jmp, _))
        | (_, (Nop, _)) -> true
        | _ -> false)
    |> Seq.map (fun (i, (op, arg)) ->
        let newOp =
            match op with
            | Jmp -> Nop
            | Nop -> Jmp
            | Acc -> Acc

        instructions
        |> Seq.toList
        |> Monke.List.replace i (newOp, arg))
    |> Seq.pick tryExecute


example |> tryFix |> printfn "Example 2: %d"


input |> tryFix |> printfn "Part Two: %d"
