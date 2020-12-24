#load "monke.fs"


let input = "day-22.input" |> Monke.IO.readToString


let parse inp =
    let both =
        inp
        |> Monke.String.splitRemoveEmpty [ "\r\n\r\n"
                                           "\n\n" ]
        |> Seq.map
            (fun inp ->
                let inp =
                    inp
                    |> Monke.String.splitRemoveEmpty [ "\r\n"
                                                       "\n" ]

                let cards = inp |> Seq.skip 1 |> Seq.map int
                cards)

    Seq.item 0 both, Seq.item 1 both


type MatchState =
    | Turn of seq<int> * seq<int>
    | Win of int64


let calculateScore deck =
    deck
    |> Seq.rev
    |> Seq.indexed
    |> Seq.sumBy (fun (i, n) -> (int64 i + 1L) * int64 n)


let step (deck1, deck2) =
    match deck1, deck2 with
    | Monke.SeqNil, deck
    | deck, Monke.SeqNil -> deck |> calculateScore |> Win

    | Monke.SeqCons (c1, deck1), Monke.SeqCons (c2, deck2) ->
        let deck1, deck2 =
            if c1 > c2 then Seq.append deck1 (seq [ c1; c2 ]), deck2 else deck1, Seq.append deck2 (seq [ c2; c1 ])

        Turn(deck1, deck2)


let rec play decks =
    match step decks with
    | Turn (deck1, deck2) -> play (deck1, deck2)
    | Win n -> n


let example = "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"


example
|> parse
|> play
|> printfn "Example 1: %d"


input |> parse |> play |> printfn "Part One: %d"


type MatchState2 =
    | Turn2 of list<int> * list<int>
    | Win1 of int64
    | Win2 of int64
    | Recur of (list<int> * list<int>) * subGame: (int * int)


let step2 (deck1, deck2) =
    match deck1, deck2 with
    | deck, [] -> Win1(calculateScore deck)
    | [], deck -> Win2(calculateScore deck)

    | c1 :: deck1, c2 :: deck2 ->
        if List.length deck1 >= c1 && List.length deck2 >= c2
        then Recur((deck1, deck2), (c1, c2))
        elif c1 > c2
        then Turn2(List.append deck1 [ c1; c2 ], deck2)
        else Turn2(deck1, List.append deck2 [ c2; c1 ])


type GameState =
    | GameWin1
    | GameWin2


let deckToString deck =
    Seq.map string deck |> String.concat ","


let rec playGame decks =
    let prev1 = Monke.HashSet.empty ()
    let prev2 = Monke.HashSet.empty ()

    let rec loop decks =
        let d1, d2 = decks
        let s1 = deckToString d1
        let s2 = deckToString d2

        if Monke.HashSet.contains s1 prev1
           && Monke.HashSet.contains s2 prev2 then
            let d1, _ = decks
            GameWin1, (calculateScore d1)
        else
            Monke.HashSet.add s1 prev1
            Monke.HashSet.add s2 prev2

            match step2 decks with
            | Win1 s -> GameWin1, s
            | Win2 s -> GameWin2, s
            | Turn2 (d1, d2) -> loop (d1, d2)
            | Recur ((deck1, deck2), (c1, c2)) ->
                let subDeck1 = List.take c1 deck1
                let subDeck2 = List.take c2 deck2

                match playGame (subDeck1, subDeck2) with
                | GameWin1, _ -> loop (List.append deck1 [ c1; c2 ], deck2)
                | GameWin2, _ -> loop (deck1, List.append deck2 [ c2; c1 ])

    loop decks


let play2 decks =
    let _, d = playGame decks
    d


example
|> parse
|> fun (d1, d2) -> Seq.toList d1, Seq.toList d2
|> play2
|> printfn "Example 2: %d"


"Player 1:
43
19

Player 2:
2
29
14"
|> parse
|> fun (d1, d2) -> Seq.toList d1, Seq.toList d2
|> play2
|> printfn "Example 3: %d"


input
|> parse
|> fun (d1, d2) -> Seq.toList d1, Seq.toList d2
|> play2
|> printfn "Part Two: %d"
