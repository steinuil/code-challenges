#load "intcode.fs"


let program = Intcode.fromFile "day-09.input"


"109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
|> Intcode.fromString
|> Intcode.evaluateGetOutput []
|> Seq.map string
|> Seq.toList
|> String.concat ","
|> printfn "Example 1: %s"


"1102,34915192,34915192,7,4,7,99,0"
|> Intcode.fromString
|> Intcode.evaluateGetOutput []
|> Seq.exactlyOne
|> printfn "Example 2: %d"


"104,1125899906842624,99"
|> Intcode.fromString
|> Intcode.evaluateGetOutput []
|> Seq.exactlyOne
|> printfn "Example 3: %d"


program
|> Intcode.evaluateGetOutput [ 1L ]
|> Seq.exactlyOne
|> printfn "Part One: %d"


program
|> Intcode.evaluateGetOutput [ 2L ]
|> Seq.exactlyOne
|> printfn "Part Two: %d"
