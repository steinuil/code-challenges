#load "intcode.fs"

let program = Intcode.fromFile "day-05.input"

program
|> Intcode.evaluate [ 1L ] (fun output -> if output <> 0L then printfn "Part One: %d" output)

program
|> Intcode.evaluate [ 5L ] (printfn "Part Two: %d")
