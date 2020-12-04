module Intcode


type Program = int64 list


type Position = int


type Param =
    | Position of Position
    | Immediate of int64
    | RelativePosition of Position


type Instruction =
    | Add of Param * Param * Param
    | Multiply of Param * Param * Param
    | Input of Param
    | Output of Param
    | JumpIfTrue of Param * Param
    | JumpIfFalse of Param * Param
    | LessThan of Param * Param * Param
    | Equals of Param * Param * Param
    | SetBase of Param
    | Halt


type ProgramState =
    { Program: Program
      Input: int64 list
      Counter: int
      Base: int }

module ProgramState =
    let make input program =
        { Program = program
          Input = input
          Counter = 0
          Base = 0 }


let private compileOpcode instr =
    let opcode = instr % 100L
    let modes = instr / 100L
    opcode, modes

let private compileParameter p modes =
    let restModes = modes / 10L

    let param =
        match modes % 10L with
        | 0L -> Position(int p)
        | 1L -> Immediate p
        | 2L -> RelativePosition(int p)
        | n -> invalidArg "mode" (string n)

    param, restModes

let private param p modes =
    let param, _ = compileParameter p modes
    param

let private param2 p1 p2 modes =
    let p1, modes = compileParameter p1 modes
    let p2, _ = compileParameter p2 modes
    p1, p2

let private param3 p1 p2 p3 modes =
    let p1, modes = compileParameter p1 modes
    let p2, modes = compileParameter p2 modes
    let p3, _ = compileParameter p3 modes
    p1, p2, p3


let compileSingleInstruction =
    function
    | [] -> failwith "unexpected end of program"
    | opcode :: rest ->
        match compileOpcode opcode, rest with
        | (1L, m), o1 :: o2 :: out :: _ ->
            let o1, o2, out = param3 o1 o2 out m
            Add(o1, o2, out), 4

        | (2L, m), o1 :: o2 :: out :: _ ->
            let o1, o2, out = param3 o1 o2 out m
            Multiply(o1, o2, out), 4

        | (3L, m), out :: _ ->
            let out = param out m
            Input out, 2

        | (4L, m), p :: _ ->
            let p = param p m
            Output p, 2

        | (5L, m), cond :: v :: _ ->
            let cond, v = param2 cond v m
            JumpIfTrue(cond, v), 3

        | (6L, m), cond :: v :: _ ->
            let cond, v = param2 cond v m
            JumpIfFalse(cond, v), 3

        | (7L, m), o1 :: o2 :: out :: _ ->
            let o1, o2, out = param3 o1 o2 out m
            LessThan(o1, o2, out), 4

        | (8L, m), o1 :: o2 :: out :: _ ->
            let o1, o2, out = param3 o1 o2 out m
            Equals(o1, o2, out), 4

        | (9L, m), p :: _ ->
            let p = param p m
            SetBase(p), 2

        | (99L, _), _ -> Halt, 1

        | (opcode, _), _ -> invalidArg "opcode" (string opcode)


let private listItem at ls =
    List.tryItem at ls |> Option.defaultValue 0L

let replace at v ls =
    let rec loop count acc =
        function
        | _ :: tl when count < 1 -> List.append (List.rev acc) (v :: tl)
        | hd :: tl -> loop (count - 1) (hd :: acc) tl
        | [] ->
            List.rev acc
            @ [ for _ in 1 .. count -> 0L ]
            @ [ v ]

    loop at [] ls

let private replaceParam at v b ls =
    let at =
        match at with
        | Position at -> at
        | RelativePosition at -> b + at
        | Immediate _ -> failwith "Parameters that an instruction writes to will never be in immediate mode"

    replace at v ls


let private evalParam program b =
    function
    | Immediate n -> n
    | Position p -> listItem p program
    | RelativePosition p -> listItem (b + p) program


exception InputRequiredException


type StepResult =
    | StepOk of ProgramState * int64 option
    | NeedsInput
    | Halted


let step { Program = program; Input = input; Counter = pc; Base = b } =
    let rest = List.skip pc program
    let instr, read = compileSingleInstruction rest
    match instr with
    | Add (o1, o2, out) ->
        let v1 = evalParam program b o1
        let v2 = evalParam program b o2
        let program = replaceParam out (v1 + v2) b program
        StepOk
            ({ Program = program
               Input = input
               Counter = pc + read
               Base = b },
             None)

    | Multiply (o1, o2, out) ->
        let v1 = evalParam program b o1
        let v2 = evalParam program b o2
        let program = replaceParam out (v1 * v2) b program
        StepOk
            ({ Program = program
               Input = input
               Counter = pc + read
               Base = b },
             None)

    | Input out ->
        match input with
        | [] -> NeedsInput
        | v :: input ->
            let program = replaceParam out v b program
            StepOk
                ({ Program = program
                   Input = input
                   Counter = pc + read
                   Base = b },
                 None)

    | Output v ->
        let v = evalParam program b v
        StepOk
            ({ Program = program
               Input = input
               Counter = pc + read
               Base = b },
             Some v)

    | JumpIfTrue (cond, v) ->
        let cond = evalParam program b cond
        if cond <> 0L then
            let pc = evalParam program b v
            StepOk
                ({ Program = program
                   Input = input
                   Counter = int pc
                   Base = b },
                 None)
        else
            StepOk
                ({ Program = program
                   Input = input
                   Counter = pc + read
                   Base = b },
                 None)

    | JumpIfFalse (cond, v) ->
        let cond = evalParam program b cond
        if cond = 0L then
            let pc = evalParam program b v
            StepOk
                ({ Program = program
                   Input = input
                   Counter = int pc
                   Base = b },
                 None)
        else
            StepOk
                ({ Program = program
                   Input = input
                   Counter = pc + read
                   Base = b },
                 None)

    | LessThan (o1, o2, out) ->
        let o1 = evalParam program b o1
        let o2 = evalParam program b o2

        let program =
            replaceParam out (if o1 < o2 then 1L else 0L) b program

        StepOk
            ({ Program = program
               Input = input
               Counter = pc + read
               Base = b },
             None)

    | Equals (o1, o2, out) ->
        let o1 = evalParam program b o1
        let o2 = evalParam program b o2

        let program =
            replaceParam out (if o1 = o2 then 1L else 0L) b program

        StepOk
            ({ Program = program
               Input = input
               Counter = pc + read
               Base = b },
             None)

    | SetBase newBase ->
        let newBase = evalParam program b newBase

        StepOk
            ({ Program = program
               Input = input
               Counter = pc + read
               Base = b + int newBase },
             None)

    | Halt -> Halted


let rec eval out state =
    match step state with
    | StepOk (state, output) ->
        Option.iter out output
        eval out state
    | NeedsInput -> state, false
    | Halted -> state, true


open System.IO


let readLine (path: string) =
    use reader = new StreamReader(path)
    reader.ReadLine()

let fromString (str: string) =
    str.Split(',') |> Seq.map int64 |> Seq.toList

let fromFile (path: string) = readLine path |> fromString
