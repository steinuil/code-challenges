let compile = function
  | 1 :: p1 :: p2 :: out :: rest ->
    `Add (p1, p2, out), rest
  | 2 :: p1 :: p2 :: out :: rest ->
    `Multiply (p1, p2, out), rest
  | 99 :: rest ->
    `Halt, rest
  | _ ->
    failwith "invalid program"
    
let replace ~at ~v ls =
  let rec loop count acc = function
    | _ :: tl when count < 1 ->
      List.rev_append acc (v :: tl)
    | hd :: tl ->
      loop (count - 1) (hd :: acc) tl
    | [] ->
      invalid_arg "list overflow"
  in
  loop at [] ls
  
let rec eva program rest =
  match compile rest with
  | `Add (p1, p2, out), rest ->
    let v1 = List.nth program p1 in
    let v2 = List.nth program p2 in
    let program = program |> replace ~at:out ~v:(v1 + v2) in
    eva program rest
  | `Multiply (p1, p2, out), rest ->
    let v1 = List.nth program p1 in
    let v2 = List.nth program p2 in
    let program = program |> replace ~at:out ~v:(v1 * v2) in
    eva program rest
  | `Halt, _ ->
    program
    
let rec print_program = function
  | [] -> ()
  | n :: [] -> print_int n
  | n :: rest -> print_int n; print_char ','; print_program rest
  
let execute program ~noun ~verb =
  let program =
    program
    |> replace ~at:1 ~v:noun
    |> replace ~at:2 ~v:verb
  in
  eva program program |> List.hd

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