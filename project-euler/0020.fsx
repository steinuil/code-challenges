let factorial (n : int) =
  let n = bigint n
  seq { 1I .. n }
  |> Seq.fold ( * ) 1I

let rec digitsOf (n : bigint) =
  seq {
    yield int (n % 10I);
    let n = n / 10I
    if n <> 0I then
      yield! digitsOf n
  }

do
  factorial 100
  |> digitsOf
  |> Seq.fold ( + ) 0
  |> printfn "%A"