open System


let LIMIT = 28123


let rec properDivisorsLoop (n: int) (i: int) (lastReciprocal: int) = seq {
    if i <> lastReciprocal then
        let d, r = Math.DivRem(n, i)
        if r = 0 then
            yield i
            if i <> d then
                yield d
                yield! properDivisorsLoop n (i + 1) d
        else
            yield! properDivisorsLoop n (i + 1) lastReciprocal
}


let properDivisors (n: int) =
    Seq.append [1] (properDivisorsLoop n 2 n)


let abundantNumbers =
    seq { 12 .. LIMIT } |> Seq.filter (fun n ->
        properDivisors n |> Seq.sum > n
    )
    |> Seq.cache


let sieve = Array.create (LIMIT + 1) false


for x in abundantNumbers do
    for y in abundantNumbers do
        if x + y <= LIMIT then
            sieve.[x + y] <- true


sieve
|> Seq.mapi (fun i isSum -> if isSum then 0L else int64 i)
|> Seq.sum
|> printfn "%d"
