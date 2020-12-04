(bigint 2 <<< 999).ToString()
|> Seq.sumBy (fun c -> int64 c - 48L)
|> printfn "%d"
