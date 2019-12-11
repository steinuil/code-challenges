#mod_use "intcode.ml"


let () =
  Intcode.read_from_file "day-05.input"
  |> Intcode.evaluate ~input:[1]
  |> ignore
