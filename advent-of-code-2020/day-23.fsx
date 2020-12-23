#if INTERACTIVE
#load "monke.fs"
#endif


let parse =
    Monke.String.chars
    >> Seq.map (string >> int)
    >> Seq.toArray


let input = "487912365" |> parse


let step currIdx inp =
    let len = Array.length inp
    let min = Array.min inp
    let max = Array.max inp

    let nextThree =
        [ inp.[(currIdx + 1) % len]
          inp.[(currIdx + 2) % len]
          inp.[(currIdx + 3) % len] ]

    let rec choosePrev n =
        let prev = if n - 1 < min then max else n - 1

        if List.contains prev nextThree then choosePrev prev else Array.findIndex (fun n -> n = prev) inp

    let prevIdx = choosePrev inp.[currIdx]

    let newInp = Array.copy inp

    seq { (currIdx + 4 % len) .. (currIdx + 4)
                                 + ((prevIdx - (currIdx + 4) + len) % len) }
    |> Seq.map (fun n -> n % len)
    |> Seq.iter (fun idx -> newInp.[((idx - 3) + len) % len] <- inp.[idx])

    seq { prevIdx - 2 .. prevIdx }
    |> Seq.map (fun n -> (n + len) % len)
    |> Seq.indexed
    |> Seq.iter (fun (threeIdx, idx) -> newInp.[idx] <- List.item threeIdx nextThree)

    newInp


let toStr inp = Seq.map string inp |> String.concat ""


let play til inp =
    let len = Array.length inp

    let rec loop til currIdx inp =
        if til = 0 then
            inp
        else
            let inp = step currIdx inp
            let currIdx = (currIdx + 1) % len
            loop (til - 1) currIdx inp

    let out = loop til 0 inp

    Seq.append out out
    |> Seq.skipWhile (fun n -> n <> 1)
    |> Seq.skip 1
    |> Seq.take (len - 1)
    |> toStr


"389125467"
|> parse
|> play 10
|> printfn "Example 1: %s"

"389125467"
|> parse
|> play 100
|> printfn "Example 2: %s"

input |> play 100 |> printfn "Part One: %s"


type MutableCell<'T> =
    { Value: 'T
      mutable Tail: MutableCell<'T> }


module MutableList =
    let createRange min max =
        let rec create s acc =
            if s < min then acc else create (s - 1) { Value = s; Tail = acc }

        let last =
            { Value = max
              Tail = Unchecked.defaultof<_> }

        let head = create (max - 1) last

        head, last

    let prepend seq head =
        let rec spread s acc =
            match s with
            | Monke.SeqCons (v, rest) -> spread rest { Value = v; Tail = acc }
            | Monke.SeqNil -> acc

        spread (Seq.rev seq) head


    let circular head last = last.Tail <- head


    let skip i head =
        let rec find curr =
            function
            | cell when curr = i -> cell
            | { Tail = tail } -> find (curr + 1) tail

        find 0 head


    let item i ls =
        let { Value = value } = skip i ls
        value


    let find x =
        let rec find i =
            function
            | head when head.Value = x -> head
            | { Tail = tail } -> find (i + 1) tail

        find 0


    let foldCell n f init ls =
        let rec loop i acc head =
            if i = n then acc else loop (i + 1) (f acc head) head.Tail

        loop 0 init ls


    let fold n f init ls =
        foldCell n (fun acc head -> f acc head.Value) init ls


    let toList n ls =
        fold n (fun t h -> h :: t) [] ls |> List.rev


    let toSet n ls =
        fold n (fun acc s -> Set.add s acc) Set.empty ls


let makePart2List orig =
    let head, last = MutableList.createRange 10 1000000
    let head = MutableList.prepend orig head
    MutableList.circular head last
    head


let advanceTurns2 til len inp =
    let min = 1
    let max = len

    let cellByValue =
        MutableList.foldCell len (fun map head -> Map.add head.Value head map) Map.empty inp

    let step2 currCell =
        let nextThree = currCell.Tail

        currCell.Tail <- MutableList.skip 4 currCell

        let nextThreeSet = MutableList.toSet 3 nextThree

        let rec findPrev n =
            let prev = if n - 1 < min then max else n - 1

            if Set.contains prev nextThreeSet then findPrev prev else Map.find prev cellByValue

        let prev = findPrev currCell.Value
        (MutableList.skip 2 nextThree).Tail <- prev.Tail
        prev.Tail <- nextThree

        currCell.Tail

    let rec loop til currCell =
        if til <> 0 then
            let nextCell = step2 currCell
            loop (til - 1) nextCell

    loop til inp


let makePart1List orig =
    let head =
        MutableList.prepend
            orig
            { Value = 0
              Tail = Unchecked.defaultof<_> }

    let tail = MutableList.skip 8 head
    tail.Tail <- head
    head


let play1 til inp =
    let inp = makePart1List inp
    advanceTurns2 til 9 inp
    let inp = inp |> MutableList.toList 9

    Seq.append inp inp
    |> Seq.skipWhile ((<>) 1)
    |> Seq.skip 1
    |> Seq.take 8
    |> toStr


let play2 inp =
    let inp = makePart2List inp

    advanceTurns2 10000000 1000000 inp

    let one = inp |> MutableList.find 1

    int64 one.Tail.Value * int64 one.Tail.Tail.Value


"389125467"
|> parse
|> play1 100
|> printfn "Example 2: %s"

"389125467"
|> parse
|> play2
|> printfn "Example 3: %d"

input |> play2 |> printfn "Part Two: %d"
