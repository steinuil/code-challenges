module ProperDivisors


open System


let rec private properDivisorsLoop (n: int64) (i: int64) (lastReciprocal: int64) = seq {
    if i <> lastReciprocal then
        let d, r = Math.DivRem(n, i)
        if r = 0L then
            yield i
            if i <> d then
                yield d
                yield! properDivisorsLoop n (i + 1L) d
        else
            yield! properDivisorsLoop n (i + 1L) lastReciprocal
}


let properDivisors (n: int64) =
    Seq.append [1L] (properDivisorsLoop n 2L n)
