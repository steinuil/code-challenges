module Program


open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System


let playfieldWidth = 37
let playfieldHeight = 24

let tileSize = 32


let find2D needle (arr: 'a [,]) =
    let rec go x y =
        if y >= arr.GetLength 1 then None
        elif x >= arr.GetLength 0 then go 0 (y + 1)
        elif arr.[x, y] = needle then Some(x, y)
        else go (x + 1) y

    go 0 0


type Day13Game() as this =
    inherit Game()

    let graphics =
        new GraphicsDeviceManager(this,
                                  PreferredBackBufferWidth = playfieldWidth
                                  * tileSize,
                                  PreferredBackBufferHeight = playfieldHeight
                                  * tileSize)

    do this.IsMouseVisible <- true

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable ball = Unchecked.defaultof<Texture2D>
    let mutable block = Unchecked.defaultof<Texture2D>
    let mutable horizontalBlock = Unchecked.defaultof<Texture2D>
    let mutable wall = Unchecked.defaultof<Texture2D>
    let mutable font = Unchecked.defaultof<SpriteFont>

    let gameSpeed = 0

    let mutable ogre = false

    let mutable puckX = 18
    let mutable nextDirection = 0

    let mutable sinceLastTick = 0
    let mutable isPaused = false
    let mutable isSpacePressed = false

    let mutable score = 0
    let mutable programState = Intcode.ProgramState.make [] []

    let mutable initialProgramState = Intcode.ProgramState.make [] []

    let playfield =
        Array2D.create playfieldHeight playfieldWidth 0y


    override _.Initialize() =
        // XXX delete the next 3 lines once 3.8.1 comes out
        graphics.PreferredBackBufferWidth <- playfieldWidth * tileSize
        graphics.PreferredBackBufferHeight <- (playfieldHeight + 1) * tileSize
        graphics.ApplyChanges()

        base.Initialize()


    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
        ball <- this.Content.Load<Texture2D>(@"Content\Ball")
        block <- this.Content.Load<Texture2D>(@"Content\Block")
        horizontalBlock <- this.Content.Load<Texture2D>(@"Content\HorizontalBlock")
        wall <- this.Content.Load<Texture2D>(@"Content\Wall")
        font <- this.Content.Load<SpriteFont>(@"Content\coders_crux")

        programState <-
            Intcode.fromFile "day-13.input"
            |> Intcode.replace 0 2L
            |> Intcode.ProgramState.make [ 2L ]

        initialProgramState <- programState

        base.LoadContent()


    override _.Update(gameTime: GameTime) =
        if Keyboard.GetState().IsKeyDown(Keys.Escape)
        then this.Exit()

        // if Keyboard.GetState().IsKeyDown(Keys.Left)
        // then projectedJoystickDirection <- Math.Clamp(projectedJoystickDirection - 1, -1, 1)

        // if Keyboard.GetState().IsKeyDown(Keys.Right)
        // then projectedJoystickDirection <- Math.Clamp(projectedJoystickDirection + 1, -1, 1)

        if Keyboard.GetState().IsKeyDown(Keys.Space)
           && not isSpacePressed then
            isPaused <- not isPaused
            isSpacePressed <- true

        if Keyboard.GetState().IsKeyUp(Keys.Space)
        then isSpacePressed <- false

        if not ogre && sinceLastTick < 1 then
            let mutable commands = []

            match Intcode.eval (fun x -> commands <- x :: commands)
                      { programState with
                            Input = programState.Input @ [ int64 nextDirection ] } with
            | state, false ->
                commands
                |> List.rev
                |> Seq.chunkBySize 3
                |> Seq.iter (function
                    | [| x; y; tile |] ->
                        if x = -1L && y = 0L then
                            score <- int tile
                        else
                            playfield.[int y, int x] <- int8 tile
                            if tile = 3L then puckX <- int x
                    | _ -> failwith "unreachable")

                match find2D 4y playfield with
                | Some (_, ballX) ->
                    if puckX < ballX then nextDirection <- 1
                    elif puckX > ballX then nextDirection <- -1
                    else nextDirection <- 0

                | None -> ()


                programState <- state

            | _, true ->
                commands
                |> List.rev
                |> Seq.chunkBySize 3
                |> Seq.iter (function
                    | [| x; y; tile |] -> if x = -1L && y = 0L then score <- int tile
                    | _ -> failwith "unreachable")

                printfn "Part Two: %d" score
                ogre <- true

            sinceLastTick <- gameSpeed
        elif not ogre && not isPaused then
            sinceLastTick <- sinceLastTick - 1

        base.Update(gameTime)


    override _.Draw(gameTime: GameTime) =
        graphics.GraphicsDevice.Clear(Color.BlanchedAlmond)

        spriteBatch.Begin(blendState = BlendState.AlphaBlend, samplerState = SamplerState.PointClamp)

        let drawTile sprite x y =
            spriteBatch.Draw(sprite, Rectangle(x * tileSize, y * tileSize, tileSize, tileSize), Color.White)

        if not ogre then
            playfield
            |> Array2D.iteri (fun y x ->
                function
                | 0y -> ()
                | 1y -> drawTile wall x y
                | 2y -> drawTile block x y
                | 3y -> drawTile horizontalBlock x y
                | 4y -> drawTile ball x y
                | c -> failwith (string c))

            spriteBatch.DrawString
                (font,
                 sprintf "Score: %08d" score,
                 Vector2(10.f, float32 (playfieldHeight * tileSize + 10)),
                 Color.Black)
        else
            spriteBatch.DrawString
                (font,
                 sprintf "A winner is you: %d" score,
                 Vector2(10.f, float32 (playfieldHeight * tileSize + 10)),
                 Color.Black)

        spriteBatch.End()

        base.Draw(gameTime)


[<EntryPoint; STAThread>]
let main _argv =
    use game = new Day13Game()
    game.Run()
    0
