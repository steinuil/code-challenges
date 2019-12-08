type program = int list


type instruction =
  | Add of int * int * int
  | Mul of int * int * int
  | Put of int * int
  | Halt


let rec show = function
  | [] -> ()
  | n :: [] -> print_int n
  | n :: rest -> print_int n; print_char ','; show rest


let compile = function
  | 1 :: p1 :: p2 :: out :: rest ->
    Add (p1, p2, out), rest
  | 2 :: p1 :: p2 :: out :: rest ->
    Mul (p1, p2, out), rest
  | 3 :: from :: to_ :: rest ->
    Put (from, to_), rest
  | 99 :: rest ->
    Halt, rest
  | n :: _ ->
    failwith (Printf.sprintf "invalid instruction: %d" n)
  | [] ->
    failwith "unexpected end of program"


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


let rec evaluate program rest =
  match compile rest with
  | Add (p1, p2, out), rest ->
    let v1 = List.nth program p1 in
    let v2 = List.nth program p2 in
    let program = program |> replace ~at:out ~v:(v1 + v2) in
    evaluate program rest
  | Mul (p1, p2, out), rest ->
    let v1 = List.nth program p1 in
    let v2 = List.nth program p2 in
    let program = program |> replace ~at:out ~v:(v1 * v2) in
    evaluate program rest
  | Put (from, to_), rest ->
    let program = program |> replace ~at:to_ ~v:from in
    evaluate program rest
  | Halt, _ ->
    program


let execute program ~noun ~verb =
  let program =
    program
    |> replace ~at:1 ~v:noun
    |> replace ~at:2 ~v:verb
  in
  evaluate program program |> List.hd
