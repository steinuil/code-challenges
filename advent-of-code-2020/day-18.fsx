#load "monke.fs"

#r "nuget: FParsec"


let input = "day-18.input" |> Monke.IO.readLines


let any inp =
    match Seq.tryHead inp with
    | Some c -> Some(c, Seq.tail inp)
    | None -> None


let satisfy f inp =
    match any inp with
    | Some (c, inp) when f c -> Some(c, inp)
    | Some _
    | None -> None


let many p inp =
    let rec loop acc inp =
        match p inp with
        | Some (v, rest) -> loop (v :: acc) rest
        | None -> Some(Seq.rev acc, inp)

    loop [] inp


let map f p inp =
    match p inp with
    | None -> None
    | Some (v, inp) -> Some(f v, inp)


let bind p f inp =
    match p inp with
    | Some (v, inp) -> f v inp
    | None -> None


let return' v inp = Some(v, inp)


let fromOption f inp =
    match any inp with
    | None -> None
    | Some (v, inp) ->
        match f v with
        | Some v -> Some(v, inp)
        | None -> None


type ParserMonad() =
    member _.Bind(p, f) = bind p f
    member _.Return(v) = return' v
    member _.ReturnFrom(v) = v


let parser = ParserMonad()


let map2 f p1 p2 inp =
    match p1 inp with
    | None -> None
    | Some (v1, inp) ->
        match p2 inp with
        | None -> None
        | Some (v2, inp) -> Some(f v1 v2, inp)


let many1 p inp =
    match p inp with
    | None -> None
    | Some (v, inp) ->
        match many p inp with
        | None -> Some(Seq.singleton v, inp)
        | Some (vs, inp) -> Some(Seq.append (Seq.singleton v) vs, inp)


let or' p1 p2 inp =
    match p1 inp with
    | Some _ as out -> out
    | None -> p2 inp


let char ch inp =
    match any inp with
    | None -> None
    | Some (c, inp) when c = ch -> Some(c, inp)
    | Some _ -> None


let digit inp =
    match any inp with
    | None -> None
    | Some (c, inp) when c >= '0' && c <= '9' -> Some(c, inp)
    | Some _ -> None


let pInt =
    many1 digit
    |> map Monke.String.fromChars
    |> map int


type Token =
    | MultiplyTok
    | SumTok
    | Lparen
    | Rparen
    | Ws
    | IntTok of int


let op =
    char '+'
    |> map (fun _ -> SumTok)
    |> or' (char '*' |> map (fun _ -> MultiplyTok))


let ws1 = many1 (char ' ')


let rec fix p inp = p (fix p) inp


let token =
    map IntTok pInt
    |> or' op
    |> or' (map (fun _ -> Ws) (char ' '))
    |> or' (map (fun _ -> Lparen) (char '('))
    |> or' (map (fun _ -> Rparen) (char ')'))


let run p inp =
    match p inp with
    | Some (v, s) when Seq.isEmpty s -> v
    | Some (v, _) -> failwithf "input not fully consumed: %A" v
    | None -> failwith "parser failed"


let tokenize (s: string) =
    run (many token) (Seq.cast s)
    |> Seq.filter ((<>) Ws)
    |> Seq.map
        (function
        | Lparen -> Rparen
        | Rparen -> Lparen
        | t -> t)
    |> Seq.rev


type Op =
    | Multiply
    | Sum


type Expr =
    | Binop of Expr * Op * Expr
    | Int of int


let expr =
    fix
        (fun expr ->
            let eInt =
                fromOption
                    (function
                    | IntTok n -> Some n
                    | _ -> None)
                |> map Int

            let eParen =
                parser {
                    let! _ = satisfy ((=) Lparen)
                    let! e = expr
                    let! _ = satisfy ((=) Rparen)
                    return e
                }

            let eTerm = eParen |> or' eInt

            parser {
                let! e1 = eTerm

                let! op =
                    fromOption
                        (function
                        | MultiplyTok -> Some Multiply
                        | SumTok -> Some Sum
                        | _ -> None)

                let! e2 = eTerm |> or' expr
                return (Binop(e1, op, e2))
            })


let evalOp =
    function
    | Multiply -> (*)
    | Sum -> (+)


let rec evalExpr =
    function
    | Int n -> int64 n
    | Binop (e1, op, e2) -> (evalOp op) (evalExpr e1) (evalExpr e2)


tokenize "(1 + 2 * 3 + 4 * 5) + 6"
|> run expr
|> evalExpr
|> printfn "Example 1: %d"


tokenize "1 + (2 * 3) + (4 * (5 + 6))"
|> run expr
|> evalExpr
|> printfn "Example 2: %d"


Seq.sumBy (tokenize >> run expr >> evalExpr) input
|> printfn "Part One: %d"


module Precedence =
    open FParsec


    let ws = spaces


    let opp =
        new OperatorPrecedenceParser<Expr, unit, unit>()

    let addSymbolicInfixOperator prefix precedence associativity =
        let op =
            InfixOperator(
                prefix,
                ws,
                precedence,
                associativity,
                (),
                fun remOpChars expr1 expr2 ->
                    Binop(
                        expr1,
                        (match prefix with
                         | "*" -> Multiply
                         | "+" -> Sum
                         | _ -> failwith "a"),
                        expr2
                    )
            )

        opp.AddOperator op


    addSymbolicInfixOperator "*" 10 Associativity.Right
    addSymbolicInfixOperator "+" 20 Associativity.Right

    let expr = opp.ExpressionParser

    let intTerm = pint32 .>> ws |>> Int

    let parenTerm =
        between (pchar '(' >>. ws) (pchar ')' >>. ws) expr

    opp.TermParser <- intTerm <|> parenTerm


    let parse inp =
        match run expr inp with
        | Success (res, _, _) -> res
        | Failure (err, _, _) -> failwithf "%A" err


Precedence.parse "1 + 2 * 3 + 4 * 5 + 6"
|> evalExpr
|> printfn "Example 3: %d"

input
|> Seq.sumBy (Precedence.parse >> evalExpr)
|> printfn "Part Two: %d"
