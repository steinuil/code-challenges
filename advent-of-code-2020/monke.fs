module Monke


open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions


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

    let splitLines = split [ "\r\n"; "\n" ]

    let splitRemoveEmpty (seps: string seq) (string: string) =
        string.Split(Seq.toArray seps, StringSplitOptions.RemoveEmptyEntries)

    let splitInTwo (sep: string) (string: string) =
        let i = string.IndexOf(sep)
        string.[0..i - 1], string.[i + 1..]

    let chars (string: String) = string.ToCharArray()

    let fromChars: seq<char> -> String = String.Concat


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


let (|RegexMatch|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None


module Dict =
    let empty (): Dictionary<'k, 'v> = new Dictionary<'k, 'v>()

    let tryFind key (dict: Dictionary<_, _>) =
        match dict.TryGetValue key with
        | true, v -> Some v
        | false, _ -> None

    let add key value (dict: Dictionary<_, _>) = dict.[key] <- value


let memo f =
    let cache = Dict.empty ()

    fun inp ->
        match Dict.tryFind inp cache with
        | Some v -> v
        | None ->
            let v = f inp
            Dict.add inp v cache
            v


let memoRec f =
    let cache = Dict.empty ()

    let rec memoized inp =
        match Dict.tryFind inp cache with
        | Some v -> v
        | None ->
            let v = f memoized inp
            Dict.add inp v cache
            v

    memoized


let flip f a b = f b a


module List =
    let replace at v ls =
        let rec loop count acc =
            function
            | _ :: tl when count < 1 -> List.append (List.rev acc) (v :: tl)
            | hd :: tl -> loop (count - 1) (hd :: acc) tl
            | [] -> failwith "invalid length"

        loop at [] ls


module Array2D =
    let toSeq (arr: 'a [,]) =
        seq {
            for y in 0 .. Array2D.length1 arr - 1 do
                for x in 0 .. Array2D.length2 arr - 1 do
                    yield arr.[y, x]
        }


let byteToBaseString (bas: int) (n: byte) = Convert.ToString(n, bas)
let int16ToBaseString (bas: int) (n: int16) = Convert.ToString(n, bas)
let intToBaseString (bas: int) (n: int) = Convert.ToString(n, bas)
let int64ToBaseString (bas: int) (n: int64) = Convert.ToString(n, bas)


let invalidInput inp = failwithf "invalid input: %A" inp
