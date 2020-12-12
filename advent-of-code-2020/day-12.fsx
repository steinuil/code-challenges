#load "monke.fs"


type Command =
    | North
    | South
    | East
    | West
    | Left
    | Right
    | Forward


type Direction =
    | U
    | D
    | L
    | R


type State1 = { Dir: Direction; X: int; Y: int }


let parseDir (inp: string) =
    let cmd =
        match inp.[0] with
        | 'N' -> North
        | 'S' -> South
        | 'E' -> East
        | 'W' -> West
        | 'L' -> Left
        | 'R' -> Right
        | 'F' -> Forward
        | x -> failwithf "invalid command: %c" x

    let dist = inp.[1..] |> int

    cmd, dist


let input =
    "day-12.input"
    |> Monke.IO.readLines
    |> Seq.map parseDir


let example1 =
    "F10
N3
F7
R90
F11"
    |> Monke.String.splitLines
    |> Seq.map parseDir


let turn90 =
    function
    | U -> R
    | R -> D
    | D -> L
    | L -> U

let turn180 = turn90 >> turn90

let turn270 = turn90 >> turn90 >> turn90


let step state =
    function
    | North, n -> { state with Y = state.Y + n }
    | South, n -> { state with Y = state.Y - n }
    | East, n -> { state with X = state.X + n }
    | West, n -> { state with X = state.X - n }
    | Left, 90
    | Right, 270 -> { state with Dir = turn270 state.Dir }
    | Left, 180
    | Right, 180 -> { state with Dir = turn180 state.Dir }
    | Left, 270
    | Right, 90 -> { state with Dir = turn90 state.Dir }
    | Left, d
    | Right, d -> failwithf "invalid deg: %d" d
    | Forward, n ->
        match state.Dir with
        | U -> { state with Y = state.Y + n }
        | D -> { state with Y = state.Y - n }
        | R -> { state with X = state.X + n }
        | L -> { state with X = state.X - n }


let initial = { Dir = R; X = 0; Y = 0 }


example1
|> Seq.fold step initial
|> fun { X = x; Y = y } -> abs x + abs y
|> printfn "Example 1: %d"


input
|> Seq.fold step initial
|> fun { X = x; Y = y } -> abs x + abs y
|> printfn "Part One: %d"


type State2 =
    { WaypointRelX: int
      WaypointRelY: int
      ShipX: int
      ShipY: int }


let step2 state =
    function
    | North, n ->
        { state with
              WaypointRelX = state.WaypointRelX + n }
    | South, n ->
        { state with
              WaypointRelX = state.WaypointRelX - n }
    | East, n ->
        { state with
              WaypointRelY = state.WaypointRelY + n }
    | West, n ->
        { state with
              WaypointRelY = state.WaypointRelY - n }
    | Left, 90
    | Right, 270 ->
        { state with
              WaypointRelX = state.WaypointRelY
              WaypointRelY = -state.WaypointRelX }
    | Left, 180
    | Right, 180 ->
        { state with
              WaypointRelX = -state.WaypointRelX
              WaypointRelY = -state.WaypointRelY }
    | Left, 270
    | Right, 90 ->
        { state with
              WaypointRelX = -state.WaypointRelY
              WaypointRelY = state.WaypointRelX }
    | Forward, n ->
        { state with
              ShipX = state.ShipX + state.WaypointRelX * n
              ShipY = state.ShipY + state.WaypointRelY * n }
    | _ -> failwith "unreachable"


let initial2 =
    { WaypointRelX = 1
      WaypointRelY = 10
      ShipX = 0
      ShipY = 0 }


example1
|> Seq.fold step2 initial2
// |> Seq.fold (fun state cmd ->
//     let state = step2 state cmd
//     printfn "%A" state
//     state) initial2
|> fun { ShipX = x; ShipY = y } -> abs x + abs y
|> printfn "Example 2: %d"


input
|> Seq.fold step2 initial2
|> fun { ShipX = x; ShipY = y } -> abs x + abs y
|> printfn "Part Two: %d"
