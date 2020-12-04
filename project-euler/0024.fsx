open System


let fact n = seq { 2 .. n } |> Seq.fold ( * ) 1


let nthPermutation x nth =
    let nth = nth - 1
    seq { x .. -1 .. 1 }
    |> Seq.fold (fun (acc, nth, digitsLeft) x ->
        let permutations = fact x
        let y, nth = Math.DivRem(nth, permutations / x)
        let n = List.item y digitsLeft
        let digitsLeft = digitsLeft |> List.filter (fun d -> d <> n)
        (acc * 10L + int64 n, nth, digitsLeft)
    ) (0L, nth, seq { 0 .. x - 1 } |> Seq.toList)
    |> fun (n, _, _) -> n


nthPermutation 10 1_000_000 |> printfn "%d"
