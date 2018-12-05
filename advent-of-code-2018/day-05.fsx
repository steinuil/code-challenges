open System
open System.IO

let fileToCharList f =
  let s = File.ReadAllText f
  s.ToCharArray ()
  |> List.ofArray

let rec react acc : char list -> char list = function
  | [] -> acc
  | '\n' :: [] -> acc
  | c1 :: c2 :: cdr
    when c1 <> c2 && Char.ToUpper c1 = Char.ToUpper c2 ->
    react acc cdr
  | c :: rest ->
    react (c :: acc) rest

let rec reactUntilFixpoint (p1 : char list) =
  let p1Len = p1.Length
  let p2 = react [] p1
  if p1Len = p2.Length then
    p1Len
  else
    reactUntilFixpoint p2

let removeChar (str : char list) char =
  let char = Char.ToLower char
  let rec rem acc = function
    | [] -> acc
    | c :: rest when Char.ToLower c = char -> rem acc rest
    | c :: rest -> rem (c :: acc) rest
  rem [] str

do
  let poly = fileToCharList "day-05.input"
  reactUntilFixpoint poly |> printfn "%d";
  seq { 'a' .. 'z' }
  |> Seq.map (fun char -> removeChar poly char |> reactUntilFixpoint)
  |> Seq.min
  |> printfn "%d"