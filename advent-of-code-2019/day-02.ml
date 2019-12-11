#mod_use "./intcode.ml"

let execute program ~noun ~verb =
  let program =
    program
    |> Intcode.replace ~at:1 ~v:noun
    |> Intcode.replace ~at:2 ~v:verb
  in
  Intcode.evaluate program |> List.hd

let find_output output program =
  let rec loop noun verb =
    if execute program ~noun ~verb = output then
      100 * noun + verb
    else if verb < 99 then
      loop noun (verb + 1)
    else if noun < 99 then
      loop (noun + 1) 0
    else
      invalid_arg "output"
  in
  loop 0 0

let () =
  let f = open_in "day-02.input" in
  let program =
    f
    |> input_line
    |> String.split_on_char ','
    |> List.map int_of_string
  in
  close_in f;
  execute program ~noun:12 ~verb:2 |> print_int; print_newline ();
  find_output 19690720 program |> print_int; print_newline ()
