#load "intcode.fs"


let program = Intcode.fromFile "day-13.input"


program
|> Intcode.evaluateGetOutput []
|> Seq.chunkBySize 3
|> Seq.filter (function
    | [| _x; _y; tile |] -> tile = 2L
    | _ -> failwith "unreachable")
|> Seq.length
|> printfn "Part One: %d"


type TileId =
    | Empty
    | Wall
    | Block
    | HorizontalBlock
    | Ball


type Tile = { Id: TileId; X: int; Y: int }


type Msg =
    | Tile of Tile
    | Score of int


let tileOfId =
    function
    | 0L -> Empty
    | 1L -> Wall
    | 2L -> Block
    | 3L -> HorizontalBlock
    | 4L -> Ball
    | n -> invalidArg "tile" (string n)


let parseMsg x y tile =
    if x = -1L && y = 0L then
        Score(int tile)
    else
        Tile
            { Id = tileOfId tile
              X = int x
              Y = int y }


let tileIdToString =
    function
    | Empty -> " "
    | Wall -> "*"
    | Block -> "#"
    | HorizontalBlock -> "-"
    | Ball -> "o"


let csi = "\x1b\x5b"

let clearScreen = csi + "2J"

let cursorPosition x y = sprintf "%s%d;%dH" csi (y + 1) (x + 1)


printf "%s%sScore: 00000000" clearScreen (cursorPosition 0 0)


program
|> Intcode.replace 0 2L
|> Intcode.evaluateGetOutput [ for _ in 0 .. 6000 -> 0L ]
|> Seq.chunkBySize 3
|> Seq.map (function
    | [| x; y; tile |] -> parseMsg x y tile
    | _ -> failwith "unreachable")
|> Seq.fold (fun prevScore ->
    function
    | Tile { Id = id; X = x; Y = y } ->
        printf "%s%s" (cursorPosition x (y + 1)) (tileIdToString id)
        prevScore
    | Score score ->
        printf "%s%08d" (cursorPosition 6 0) score
        if score <> 0 then score else prevScore) 0
|> printfn "%s%sScore: %d" clearScreen (cursorPosition 0 0)
