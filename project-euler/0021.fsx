#load "./ProperDivisors.fs"


open ProperDivisors


let mutable sum = 0L
let toSkip = Array.create 10000 false


#time
toSkip |> Array.iteri (fun i shouldSkip ->
    let n = int64 (i + 1)
    if not shouldSkip then
        let reciprocal = properDivisors n |> Seq.sumBy int64
        if reciprocal - 1L < 10000L then
            toSkip.[int reciprocal - 1] <- true
        if properDivisors reciprocal |> Seq.sumBy int64 = n then
            sum <- sum + n + reciprocal
)

// for i in 1 .. 10000 do
//     if not <| amicable.Contains i then
//         let reciprocal = properDivisors n |> Seq.sum 

// seq { 1 .. 10000 }
// |> Seq.fold (fun amicable n ->


//     if Set.contains n amicable then amicable else
//     let reciprocal = properDivisors n |> Seq.sum
//     if properDivisors reciprocal |> Seq.sum = n then
//         sum <- sum + n + reciprocal
//         amicable |> Set.add reciprocal
//     else amicable
// ) Set.empty

printfn "%d" sum
