open Containers

type node = Node of string * node list * int list

let make_name =
  let a = Char.code 'A' in
  let z = Char.code 'Z' in
  let char_of c n =
    Format.sprintf "%c%d"
      (Char.chr (c + a))
      n
  in
  let letter = ref 0 in
  let suffix = ref 1 in
  fun () ->
    if !letter > z - a then begin
      letter := 0;
      incr suffix
    end;
    let name = char_of !letter !suffix in
    incr letter;
    name

let read_node ls =
  let rec read_children (children, rest) _ =
    let node, rest = read_nodes rest in
    node :: children, rest

  and read_nodes = function
    | children_n :: entries_n :: rest ->
      let name = make_name () in
      let children, rest =
        List.range' 0 children_n
        |> List.fold_left read_children ([], rest)
      in
      let children = List.rev children in
      let entries = List.take entries_n rest in
      let rest = List.drop entries_n rest in
      Node (name, children, entries), rest

    | el :: [] -> invalid_arg (string_of_int el)
    | _ -> invalid_arg "aaaa"
  in
  let tree, rest = read_nodes ls in
  match rest with
  | [] -> tree
  | _ -> invalid_arg "what"

let sum_list = List.fold_left ( + ) 0

let rec sum_metadata (Node (_, children, entries)) =
  let sum = sum_list entries in
  let sum_children =
    List.map sum_metadata children |> sum_list
  in
  sum + sum_children

let rec node_value (Node (name, children, entries)) =
  let sum_children children idx =
    match List.nth_opt children (idx - 1) with
    | Some node -> node_value node
    | None -> 0
  in
  match children with
  | [] -> sum_list entries
  | _ ->
    List.map (sum_children children) entries
    |> sum_list

let slurp_line f =
  IO.with_in f IO.read_line

let () =
  let nodes =
    slurp_line "day-08.input"
    |> Option.get_exn
    |> String.split_on_char ' '
    |> List.map int_of_string
    |> read_node
  in
  sum_metadata nodes
  |> Format.printf "Part 1: %d\n";
  node_value nodes
  |> Format.printf "Part 2: %d\n"
