let rec loop n x = seq {
    yield n + x
    yield n + x * 2L
    yield n + x * 3L
    yield n + x * 4L

    yield! loop (n + x * 4L) (x + 2L)
}


let spiral = Seq.append [1L] <| loop 1L 2L


spiral
|> Seq.takeWhile (fun x -> x <= (1001L * 1001L))
|> Seq.sum
|> printfn "%d"
