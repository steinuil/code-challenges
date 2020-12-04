module Monke


open System
open System.IO


module IO =
    let readToString (path: string) =
        use reader = new StreamReader(path)
        reader.ReadToEnd()

    let readLines (path: string) =
        seq {
            use reader = new StreamReader(path)
            while not reader.EndOfStream do
                yield reader.ReadLine()
        }


module String =
    let split (seps: string seq) (string: string) =
        string.Split(Seq.toArray seps, StringSplitOptions.None)

    let splitRemoveEmpty (seps: string seq) (string: string) =
        string.Split(Seq.toArray seps, StringSplitOptions.RemoveEmptyEntries)

    let splitInTwo (sep: string) (string: string) =
        let i = string.IndexOf(sep)
        string.[0..i - 1], string.[i + 1..]


type OptionMonad() =
    member _.Bind(t, f) = Option.bind f t
    member _.Return(t) = Some t
    member _.ReturnFrom(t) = t


let option = OptionMonad()


let tryParseInt (s: string) =
    match Int32.TryParse(s) with
    | true, n -> Some n
    | false, _ -> None


let tryParseHexByte (s: string): option<uint8> =
    try
        Convert.ToByte(s, 16) |> Some
    with _ -> None


module Option =
    let check f =
        Option.bind (fun t -> if f t then Some t else None)
