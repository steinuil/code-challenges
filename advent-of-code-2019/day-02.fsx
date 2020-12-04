#load "intcode.fs"

let program = Intcode.fromFile "day-02.input"

let execute noun verb =
    Intcode.replace 1 noun
    >> Intcode.replace 2 verb
    >> Intcode.evaluate [] ignore
    >> List.head

program
|> execute 12L 2L
|> printfn "Part One: %d"

let findOutput output program =
    let rec loop noun verb =
        if execute noun verb program = output then 100L * noun + verb
        else if verb < 99L then loop noun (verb + 1L)
        else if noun < 99L then loop (noun + 1L) 0L
        else invalidArg "output" (string output)

    loop 0L 0L

program
|> findOutput 19690720L
|> printfn "Part Two: %d"
