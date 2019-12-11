#mod_use "intcode.ml"


let () =
  let program = Intcode.read_from_file "day-05.input" in
  program |> Intcode.evaluate ~input:[1] |> ignore;
  program |> Intcode.evaluate ~input:[5] |> ignore
