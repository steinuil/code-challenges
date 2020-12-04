let isLeapYear year =
    year % 4 = 0 && (year % 100 <> 0 || year % 400 = 0)


// missing december
let monthLengths year =
    if isLeapYear year then
        [31;29;31;30;31;30;31;31;30;31;30]
    else
        [31;28;31;30;31;30;31;31;30;31;30]


let startingDay year =
    let leapYears = (year - 1901) / 4
    let years = year - 1899
    (years + leapYears) % 7


let sundaysOnFirstOfTheMonth year =
    let starting = startingDay year
    monthLengths year
    |> List.fold (fun (a, b) x ->
        if (a - starting + x) % 7 = 6 then
            (a + x, b + 1)
        else
            (a + x, b)

    ) (0, if starting = 6 then 1 else 0)
    |> snd


seq { 1901 .. 2000 }
|> Seq.sumBy sundaysOnFirstOfTheMonth
|> printfn "%d"
