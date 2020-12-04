#load "monke.fs"


open System.Text.RegularExpressions


let input = Monke.IO.readToString "day-04.input"


let parsePassport1 fields =
    let map =
        fields
        |> Seq.map (Monke.String.splitInTwo ":")
        |> Map.ofSeq

    Monke.option {
        let! birthYear = Map.tryFind "byr" map
        let! birthYear = Monke.tryParseInt birthYear
        let! issueYear = Map.tryFind "iyr" map
        let! issueYear = Monke.tryParseInt issueYear
        let! expirationYear = Map.tryFind "eyr" map
        let! expirationYear = Monke.tryParseInt expirationYear
        let! height = Map.tryFind "hgt" map
        let! hairColor = Map.tryFind "hcl" map
        let! eyeColor = Map.tryFind "ecl" map
        let! passportId = Map.tryFind "pid" map
        let countryId = Map.tryFind "cid" map

        return (birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportId, countryId)
    }


let parsePassports =
    Monke.String.split [ "\n\n"
                         "\r\n\r\n" ]
    >> Seq.map (Monke.String.split [ " "; "\n"; "\r\n" ])


input
|> parsePassports
|> Seq.map parsePassport1
|> Seq.choose id
|> Seq.length
|> printfn "Part One: %d"


[<RequireQualifiedAccess>]
type EyeColor =
    | Amber
    | Blue
    | Brown
    | Gray
    | Green
    | Hazel
    | Other


[<RequireQualifiedAccess>]
type Height =
    | Cm of int
    | In of int


type Color = { R: uint8; G: uint8; B: uint8 }


type Passport =
    { BirthYear: int
      IssueYear: int
      ExpirationYear: int
      Height: Height
      HairColor: Color
      EyeColor: EyeColor
      PassportId: int
      CountryId: int option }


let parseHeight height =
    let matches = Regex.Match(height, "^([0-9]+)(cm|in)$")

    if not matches.Success then
        None
    else
        try
            let h =
                matches.Groups.[1].Captures.[0].Value
                |> Monke.tryParseInt

            let unit = matches.Groups.[2].Captures.[0].Value

            match h, unit with
            | Some h, "cm" when h >= 150 && h <= 193 -> Some(Height.Cm h)
            | Some h, "in" when h >= 59 && h <= 76 -> Some(Height.In h)
            | _ -> None
        with _ -> None


let parseColor color =
    let matches =
        Regex.Match(color, "^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$")

    if not matches.Success then
        None
    else
        try
            Monke.option {
                let! r =
                    matches.Groups.[1].Captures.[0].Value
                    |> Monke.tryParseHexByte

                let! g =
                    matches.Groups.[2].Captures.[0].Value
                    |> Monke.tryParseHexByte

                let! b =
                    matches.Groups.[3].Captures.[0].Value
                    |> Monke.tryParseHexByte

                return { R = r; G = g; B = b }
            }
        with _ -> None


let parseEyeColor =
    function
    | "amb" -> Some EyeColor.Amber
    | "blu" -> Some EyeColor.Blue
    | "brn" -> Some EyeColor.Brown
    | "gry" -> Some EyeColor.Gray
    | "grn" -> Some EyeColor.Green
    | "hzl" -> Some EyeColor.Hazel
    | "oth" -> Some EyeColor.Other
    | _ -> None


let isBetween min max v = v >= min && v <= max


let parsePassport fields =
    let map =
        fields
        |> Seq.map (Monke.String.splitInTwo ":")
        |> Map.ofSeq

    Monke.option {
        let! birthYear =
            Map.tryFind "byr" map
            |> Option.bind Monke.tryParseInt
            |> Monke.Option.check (isBetween 1920 2002)

        let! issueYear =
            Map.tryFind "iyr" map
            |> Option.bind Monke.tryParseInt
            |> Monke.Option.check (isBetween 2010 2020)

        let! expirationYear =
            Map.tryFind "eyr" map
            |> Option.bind Monke.tryParseInt
            |> Monke.Option.check (isBetween 2020 2030)

        let! height = Map.tryFind "hgt" map |> Option.bind parseHeight

        let! hairColor = Map.tryFind "hcl" map |> Option.bind parseColor

        let! eyeColor = Map.tryFind "ecl" map |> Option.bind parseEyeColor

        let! passportId =
            Map.tryFind "pid" map
            |> Monke.Option.check (fun pid -> pid.Length = 9)
            |> Option.bind Monke.tryParseInt

        let! countryId =
            match Map.tryFind "cid" map with
            | Some (cid) ->
                match Monke.tryParseInt cid with
                | None -> None
                | Some (cid) -> Some(Some(cid))
            | None -> Some(None)

        return { BirthYear = birthYear
                 IssueYear = issueYear
                 ExpirationYear = expirationYear
                 Height = height
                 HairColor = hairColor
                 EyeColor = eyeColor
                 PassportId = passportId
                 CountryId = countryId }
    }


input
|> parsePassports
|> Seq.map parsePassport
|> Seq.choose id
|> Seq.length
|> printfn "Part Two: %d"
