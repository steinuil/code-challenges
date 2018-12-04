type Lattice =
  | End
  | Branch of right : Lattice * down : Lattice

let countPaths side =
  let paths = ref 0UL
  let rec count x y =
    if x >= side || y >= side then
      paths := !paths + 1UL
    else
      count (x + 1) y;
      count x (y + 1)
  count 0 0;
  !paths

[<EntryPoint>]
let main args =
  int args.[0]
  |> countPaths
  |> printfn "%d";
  0
