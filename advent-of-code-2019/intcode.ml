let rec list_drop n = function
  | ls when n < 1 -> ls
  | _ :: rest ->
    list_drop (n - 1) rest
  | [] ->
    invalid_arg "n"


type program = int list


type param =
  | Position of int
  | Immediate of int


type instruction =
  | Add of param * param * int
  | Multiply of param * param * int
  | Input of int
  | Output of param
  | Halt


let instruction instr =
  let i = instr mod 100 in
  let modes = instr / 100 in
  i, modes


let parameter p modes =
  let rest = modes / 10 in
  let param =
    match modes mod 10 with
    | 0 -> Position p
    | 1 -> Immediate p
    | n -> Printf.kprintf failwith "invalid parameter mode: %d" n
  in
  param, rest


let param p modes =
  let param, _ = parameter p modes in
  param


let param2 p1 p2 modes =
  let p1, modes = parameter p1 modes in
  let p2, _ = parameter p2 modes in
  p1, p2


let param3 p1 p2 p3 modes =
  let p1, modes = parameter p1 modes in
  let p2, modes = parameter p2 modes in
  let p3, _ = parameter p3 modes in
  p1, p2, p3


let compile = function
  | [] ->
    failwith "unexpected end of program"
  | n :: rest ->
  match instruction n, rest with
  | (1, m), o1 :: o2 :: out :: _ ->
    let o1, o2 = param2 o1 o2 m in
    Add (o1, o2, out), 4

  | (2, m), o1 :: o2 :: out :: _ ->
    let o1, o2 = param2 o1 o2 m in
    Multiply (o1, o2, out), 4

  | (3, m), out :: _ ->
    Input out, 2

  | (4, m), p :: _ ->
    let p = param p m in
    Output p, 2

  | (99, _), _ ->
    Halt, 1

  | (i, _), _ ->
    Printf.ksprintf failwith "invalid program instruction: %d" n


let replace ~at ~v ls =
  let rec loop count acc = function
    | _ :: tl when count < 1 ->
      List.rev_append acc (v :: tl)
    | hd :: tl ->
      loop (count - 1) (hd :: acc) tl
    | [] ->
      invalid_arg "list too short"
  in
  loop at [] ls


let eval_param ~program = function
  | Immediate n -> n
  | Position n -> List.nth program n


let evaluate program =
  let rec evaluate program pc =
    let rest = list_drop pc program in
    match compile rest with
    | Add (p1, p2, out), read ->
      let v1 = eval_param ~program p1 in
      let v2 = eval_param ~program p2 in
      let program = program |> replace ~at:out ~v:(v1 + v2) in
      evaluate program (pc + read)

    | Multiply (p1, p2, out), read ->
      let v1 = eval_param ~program p1 in
      let v2 = eval_param ~program p2 in
      let program = program |> replace ~at:out ~v:(v1 * v2) in
      evaluate program (pc + read)

    | Input p, read ->
      evaluate program (pc + read)

    | Output p, read ->
      evaluate program (pc + read)

    | Halt, _ ->
      program
  in
  evaluate program 0


let show program =
  let rec show = function
    | [] -> print_endline "]"
    | n :: [] ->
      print_int n;
      print_endline "]"
    | n :: rest ->
      print_int n;
      print_string ", ";
      show rest
  in
  print_char '[';
  show program
