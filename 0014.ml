let one_million = 1_000_000

let seen : (int, int) Hashtbl.t =
  Hashtbl.create one_million

let () = Hashtbl.add seen 1 1

let rec collatz n acc =
  match Hashtbl.find_opt seen n with
  | None ->
      let x =
        if n mod 2 == 0
        then n / 2
        else (3 * n) + 1 in
      collatz x (n :: acc)
  | Some x ->
      List.fold_left begin fun x n ->
        let count = x + 1 in
        Hashtbl.add seen n count;
        count
      end x acc

let () =
  let max = ref (0, 0) in
  for i = 13 to one_million do
    let len = collatz i [] in
    let (_, max_len) = !max in
    if len > max_len then
      max := (i, len)
  done;
  print_int (fst !max);
  print_newline ()
