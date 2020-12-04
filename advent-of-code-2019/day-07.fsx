#load "intcode.fs"

let amplify p1 p2 p3 p4 p5 program =
    let eval phase signal =
        Intcode.evaluateGetOutput [ int64 phase; signal ] program
        |> Seq.exactlyOne

    0L
    |> eval p1
    |> eval p2
    |> eval p3
    |> eval p4
    |> eval p5

"3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
|> Intcode.fromString
|> amplify 4 3 2 1 0
|> printfn "Example 1: %d"

"3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
|> Intcode.fromString
|> amplify 0 1 2 3 4
|> printfn "Example 2: %d"

"3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
|> Intcode.fromString
|> amplify 1 0 4 3 2
|> printfn "Example 3: %d"

let findMaxSignal program =
    [ 0; 1; 2; 3; 4 ]
    |> Intcode.permutations
    |> Seq.map
        (Seq.toArray
         >> function
         | [| p1; p2; p3; p4; p5 |] -> amplify p1 p2 p3 p4 p5 program
         | _ -> failwith "unreachable")
    |> Seq.max

"3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
|> Intcode.fromString
|> findMaxSignal
|> printfn "Example 3 findMaxSignal: %d"

let program = Intcode.fromFile "day-07.input"

program |> findMaxSignal |> printfn "Part One: %d"

type FeedbackLoopState =
    | Starved
    | Halted

let feedbackLoop settings program =
    let programs =
        settings
        |> Seq.map int64
        |> Seq.toArray
        |> Array.map (fun setting -> Intcode.ProgramState.make [ setting ] program)

    let appendOutput idx output =
        let p = programs.[idx]
        programs.[idx] <- { p with Input = p.Input @ [ output ] }

    appendOutput 0 0L

    let programCount = programs.Length

    let rec evalUntilBlocked curr =
        let nextProgram = (curr + 1) % programCount

        match Intcode.evalUntil programs.[curr] with
        | programState, Intcode.HasOutput output ->
            appendOutput nextProgram output
            programs.[curr] <- programState
            evalUntilBlocked curr

        | programState, Intcode.NeedsInput ->
            programs.[curr] <- programState
            Starved

        | programState, Intcode.Halted ->
            programs.[curr] <- programState
            Halted

    let rec evalFeedbackLoop curr =
        match evalUntilBlocked curr with
        | Halted when curr = programs.Length - 1 -> programs.[0].Input |> Seq.exactlyOne
        | Halted
        | Starved -> evalFeedbackLoop ((curr + 1) % programs.Length)

    evalFeedbackLoop 0


"3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
|> Intcode.fromString
|> feedbackLoop [ 9; 8; 7; 6; 5 ]
|> printfn "Example 4: %d"

"3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
|> Intcode.fromString
|> feedbackLoop [ 9; 7; 8; 5; 6 ]
|> printfn "Example 5: %d"

let findMaxSignal2 program =
    [ 5; 6; 7; 8; 9 ]
    |> Intcode.permutations
    |> Seq.map (fun settings -> feedbackLoop settings program)
    |> Seq.max

"3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
|> Intcode.fromString
|> findMaxSignal2
|> printfn "Example 5 findMaxSignal2: %d"

program
|> findMaxSignal2
|> printfn "Part Two: %d"
