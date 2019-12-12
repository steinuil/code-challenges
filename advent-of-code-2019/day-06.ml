type orbit_tree =
  | Tip of string
  | Branch of string * orbit_tree list


let read_file fname =
  let f = open_in fname in
  let rec loop acc =
    try
      let line = input_line f in
      let x = String.sub line 0 3 in
      let y = String.sub line 4 3 in
      loop ((x, y) :: acc)
    with End_of_file ->
      List.rev acc
  in
  let input = loop [] in
  close_in f;
  input


let rec attach_to_tip x y = function
  | Tip t when t = x ->
    Some (Branch (t, [Branch (x, [y])]))

  | Branch (t, children) when t = x ->
    Some (Branch (t, (Branch (x, [y]) :: children)))

  | Tip _ -> None

  | Branch (t, children) ->
    let rec loop acc = function
      | [] -> None
      | hd :: tl ->
      match attach_to_tip x y hd with
      | None -> None
      | Some x ->
        Some (List.rev_append (x :: acc) tl)
    in
    loop [] children
    |> Option.map (fun children -> Branch (t, children))
      

    (* Branch (t, List.map (attach_tip x y) children) *)


let rec attach_to_root y = function
  | Tip t as o when t = y ->
    Some (Branch (y, [o]))

  | Branch (t, children) as o when t = y ->
    Some (Branch (y, [o]))

  | Tip _ | Branch _ -> None


let until f =
  let rec until f acc = function
    | [] -> None
    | hd :: tl ->
    match f hd with
    | Some s ->
      Some (s, List.rev_append acc tl)
    | None ->
      until f (hd :: acc) tl
  in
  until f []


let until_attach f =
  let rec until_attach f acc = function
    | [] -> None
    | hd :: tl ->
    match f hd with
    | Some s ->
      Some (List.rev_append (s :: acc) tl)
    | None ->
      until_attach f (hd :: acc) tl
  in
  until_attach f []


let construct_tree =
  let rec construct_tree partial = function
    | [] -> partial
    | (from, to_) :: rest ->
      let to_, partial =
        until (attach_to_root to_) partial
        |> Option.value ~default:(Tip to_, partial)
      in
      let x =
        until_attach (attach_to_tip from to_) partial
        |> Option.value ~default:(Branch (from, [to_]) :: partial)
      in
      construct_tree x rest
  in
  construct_tree []


let () =
  let input = read_file "day-06.input" in
  ()

