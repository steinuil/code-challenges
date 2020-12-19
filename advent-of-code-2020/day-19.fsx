#load "monke.fs"


let input = "day-19.input" |> Monke.IO.readLines


type RuleP =
    | CharP of char
    | SeqP of seq<int>
    | OrP of RuleP * RuleP


let parseSeq =
    Monke.String.split [ " " ] >> Seq.map int


let parse (inp: seq<string>) =
    let rules =
        inp
        |> Seq.takeWhile (fun line -> line.Length <> 0)

    let count = Seq.length rules

    let rules =
        rules
        |> Seq.fold
            (fun map line ->
                match line with
                | Monke.RegexMatch """^(\d+): "(.)"$""" [ num; c ] -> Map.add (int num) (CharP(char c)) map

                | Monke.RegexMatch @"^(\d+): (.+) \| (.+)$" [ num; l; r ] ->
                    let l = parseSeq l |> SeqP
                    let r = parseSeq r |> SeqP
                    Map.add (int num) (OrP(l, r)) map

                | Monke.RegexMatch @"^(\d+): (.+)$" [ num; s ] -> Map.add (int num) (SeqP(parseSeq s)) map

                | _ -> Monke.invalidInput line)
            Map.empty

    let messages = Seq.skip (count + 1) inp

    rules, messages


type RuleRegex =
    | Char of char
    | Seq of seq<RuleRegex>
    | Or of RuleRegex * RuleRegex


let build m =
    let build =
        Monke.memoRec
            (fun buildRec ->
                function
                | CharP c -> Char c
                | OrP (r1, r2) -> Or(buildRec r1, buildRec r2)
                | SeqP s -> Seq(Seq.map (fun r -> Map.find r m |> buildRec) s))

    build (Map.find 0 m)


let rec show =
    function
    | Char c -> string c
    | Or (r1, r2) -> sprintf "(%s|%s)" (show r1) (show r2)
    | Seq (r) -> Seq.map show r |> String.concat ""


open System.Text.RegularExpressions


let countMatches (rules, messages) =
    let r = build rules |> show

    messages
    |> Seq.filter (fun m -> Regex.IsMatch(m, sprintf "^%s$" r))
    |> Seq.length


"""0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"""
|> Monke.String.splitLines
|> parse
|> countMatches
|> printfn "Example 1: %d"


input
|> parse
|> countMatches
|> printfn "Part One: %d"


let eval' grammar inp =
    let rec evalRec inp rule =
        match rule with
        | CharP ch ->
            match inp with
            | Monke.SeqCons (c, rest) when c = ch -> [ rest ]
            | _ -> []

        | OrP (r1, r2) -> evalRec inp r1 @ evalRec inp r2

        | SeqP rs ->
            let rec loop rules inp =
                match rules with
                | Monke.SeqNil -> [ inp ]
                | Monke.SeqCons (r, rest) ->
                    let res = evalRec inp (Map.find r grammar)

                    List.choose
                        (fun inp ->
                            match loop rest inp with
                            | [] -> None
                            | res -> Some res)
                        res
                    |> List.concat

            loop rs inp

    (evalRec inp (Map.find 0 grammar)
     |> Seq.filter Seq.isEmpty
     |> Seq.length) = 1


let countMatches2 (rules, messages) =
    let rules =
        rules
        |> Map.add 8 (OrP(SeqP([ 42 ]), SeqP([ 42; 8 ])))
        |> Map.add 11 (OrP(SeqP([ 42; 31 ]), SeqP([ 42; 11; 31 ])))

    Seq.filter (eval' rules) messages |> Seq.length


"""42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"""
|> Monke.String.splitLines
|> parse
|> countMatches2
|> printfn "Example 2: %d"


input
|> parse
|> countMatches2
|> printfn "Part Two: %d"
