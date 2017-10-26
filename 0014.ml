let one_million = 1_000_000

let seen : (int, int) Hashtbl.t =
  Hashtbl.create one_million

let () = Hashtbl.add seen 1 1

(* let seen : int option array = Array.make one_million None
let () = Array.set seen 1 (Some 1) *)

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
  for i = 13 to one_million do
    ignore @@ collatz i []
  done;
  let (max, _) = Hashtbl.fold begin fun n count ((_, max) as acc) ->
      if count > max then (n, count) else acc
    end seen (0, 0) in
  print_int max;
  print_newline ()
