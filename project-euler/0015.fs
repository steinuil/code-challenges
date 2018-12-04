open System.Collections.Generic

// type Lattice =
//   | End
//   | Branch of right : Lattice * down : Lattice

let countPaths width height : uint64 =
  let memo = new Dictionary<(int * int), uint64>()
  let rec count x y =
    if x >= width || y >= height then
      1UL
    else
      match memo.TryGetValue ((x, y)) with
      | true, c -> c
      | false, _ ->
        let right = count (x + 1) y
        let down = count x (y + 1)
        let both = right + down
        memo.Add ((x, y), both)
        both
  count 0 0

[<EntryPoint>]
let main args =
  let (width, height) =
    match args with
    | [| width; height |] ->
      (int width, int height)
    | [| width |] ->
      let width = int width
      (width, width)
    | _ ->
      printfn "Usage: 0015 <width> [<height>]"
      exit 1
  countPaths width height
  |> printfn "%d";
  0